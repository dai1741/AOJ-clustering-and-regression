fs      = require 'fs'
msgpack = require 'msgpack'

# if process.argv.length < 3
#   console.log "Usage: coffee prog aoj-user-data-json-file out-file"
#   return 1

aojUserDataFilename = process.argv[2] ? "aoj-user-data.msg"
outputFilename = process.argv[3] ? "aoj-problem-data.msg"
minSolved = +(process.argv[4] ? 0)
maxSolved = +(process.argv[5] ? 77777)

# nodeでstdinをまとめて読むうまい方法がないのでつらい

fs.readFile aojUserDataFilename, (err, data) ->
  if err then throw err
  probData = {}  # AOJの問題IDは連続じゃないので連想配列（にしたが高々10^4程度なのでその必要はなかった）
  userData = msgpack.unpack data
  for {userId, solveds, codeSizes}, idx in userData when minSolved <= solveds.length <= maxSolved
    for probId, ci in solveds  # cs単体ではzipできないらしい
      probData[probId] ?= problemId: +probId, solvers: [], codeSizes: []
      probData[probId].solvers.push idx
      probData[probId].codeSizes.push codeSizes[ci]

  probDataList = (v for own k,v of probData)
  probDataList.sort (a,b) -> a.problemId - b.problemId

  console.log "info: There are #{probDataList.length} problems " +
    "which are solved by at least one user in the given data."

  # All data traversed. Ship it!
  fs.writeFile outputFilename, msgpack.pack(probDataList), (err) ->
    if err then throw err
    console.log "Saved to file #{outputFilename}"
