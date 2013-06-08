request = require 'request'
cheerio = require 'cheerio'
async   = require 'async'
fs      = require 'fs'
msgpack = require 'msgpack'

RANK_LIST_URL = "http://judge.u-aizu.ac.jp/onlinejudge/webservice/user_list?solved_min=10&solved_max=77777"
USER_URL      = "http://judge.u-aizu.ac.jp/onlinejudge/webservice/user?id="

# ユーザーIDで使えない文字にマッチするregex
# 実際のところ使えるのは\wのみのようですが、明記されていなかったので多めにとった
FORBIDDEN_USER_ID_REGEX = /[^a-zA-Z0-9_()*=^~\[\]!"#$%'@`;:{}<>,.\\-]/g

REQUEST_INTERVAL_MS = 1000

outputFilename = process.argv[2] ? "aoj-user-data.msg"
console.log "Destination: #{outputFilename}"

async.waterfall [
  (callback) ->
    request RANK_LIST_URL, (error, response, body) ->
      if error or response.statusCode isnt 200
        callback "Failed to load user list"
      else
        setTimeout callback, REQUEST_INTERVAL_MS, null, body

  (usersXml, callback) ->
    $ = cheerio.load usersXml, xmlMode: yes
    userIds = $('user > id').map -> @text().replace FORBIDDEN_USER_ID_REGEX, ''
    console.log "Found #{userIds.length} users"

    userData = []

    async.eachSeries userIds, ((userId, callback) ->
      request USER_URL + userId, (error, response, body) ->
        if error or response.statusCode isnt 200
          callback "Failed to load #{userId}'s user data"
        else
          $ = cheerio.load body, xmlMode: yes
          # problemIdsは重複要素なしの昇順を仮定しているがまずいかもしれない
          [problemIds, codeSizes] = for elm in ['id', 'code_size']
            $("solved_list #{elm}").map -> +@text().replace /\D/g, ''
          userData.push userId: userId, solveds: problemIds, codeSizes: codeSizes
          console.log "#{userId} solved #{problemIds.length} problems"

          setTimeout callback, REQUEST_INTERVAL_MS, null
    ), (err) ->
      callback err, userData

  (userData, callback) ->
    console.log "All data fetched. Ship it!"
    fs.writeFile outputFilename, msgpack.pack(userData), callback

], (err) ->
  if err then throw err
  console.log "Saved to file #{outputFilename}"
