fs      = require 'fs'
msgpack = require 'msgpack'
jade    = require 'jade'
less    = require 'less'

if process.argv.length < 4
  console.log "Usage: coffee prog clusters-msg-file out-html-file"
  return 1

clustersFilename = process.argv[2] ? "clusters-50.msg"
outputFilename = process.argv[3] ? "clusters-50.html"
TEMPLATE_FILENAME = "template.jade"


fs.readFile clustersFilename, (err, data) ->
  if err then throw err
  clusters = msgpack.unpack data

  # 見やすさのため番号順にクラスタをソート
  # 本当は重心ベクトルの類似度で分類したほうがいいがやり方わからない
  for cluster in clusters
    cluster.sort (a,b) -> a - b
  clusters.sort (a,b) -> a[0] - b[0]

  # 問題IDからクラスタ番号を得る配列を作る
  probs = []
  for cluster, clusterNo in clusters
    probs[id] = clusterNo for id in cluster
  # console.log clusters
  console.log probs

  parser = jade.compile(fs.readFileSync(TEMPLATE_FILENAME), pretty: no)
  html = parser
    clusters: clusters
    problems: probs
    aojProblemUrl: 'http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id='
    aojUserUrl:    'http://judge.u-aizu.ac.jp/onlinejudge/webservice/user'
    pad0: (str, len) ->
      (new Array(len).join('0') + str).slice(-len)
    renderLessSync: (styles) ->
      res = null
      less.render styles, (e, css) -> unless e then res = css
      if res? then return res
      throw new Error
  fs.writeFileSync(outputFilename, html)
  # console.log html
