fs      = require 'fs'
msgpack = require 'msgpack'
jade    = require 'jade'
less    = require 'less'

if process.argv.length < 4
  console.log "Usage: coffee prog [score-name score-msg-file]* out-html-file"
  return 1

TEMPLATE_FILENAME = "template-scores.jade"
TRAINING_SCORE_FILENAME = "r/training-scores.msg"
scoreNames = (process.argv[i] for i in [2...process.argv.length-2] when i%2 == 0)
scoreFilenames = (process.argv[i] for i in [3...process.argv.length-1] when i%2 == 1)
outputFilename = process.argv[process.argv.length-1]
unless outputFilename?.match(/\.html$/)
  console.err 'output file name must end with ".html"'
  return 2

# read score data
data = for fname in scoreFilenames
  msgpack.unpack fs.readFileSync(fname)

# gather scores
probs = []
for dat in data
  for score, i in dat.scores
    (probs[dat.problemIds[i]] ?= []).push score

# compute means
for prob in probs when prob?
  prob.unshift (prob.reduce (x,y) -> x+y) / prob.length

# search for unsolveds
# 回答者なしリストはAOJのvolume apiのデータと突き合せることを手動で繰り返すことにより作った
# ここでの回答者なしはaccept数0という意味ではないので注意
unsolvedProbs = [1334, 2047, 2072, 2077, 2159, 2174, 2387, 2388, 2395, 2477]

# ゴリ押しで教師データをぶちこむ
for prob in probs when prob?
  prob.push null
trainingScores = msgpack.unpack fs.readFileSync(TRAINING_SCORE_FILENAME)
for id, i in trainingScores.problemIds when probs[id]?
  probs[id][probs[id].length - 1] = trainingScores.scores[i]



# render scores
parser = jade.compile(fs.readFileSync(TEMPLATE_FILENAME), pretty: no)
html = parser
  scoreNames: scoreNames
  problems: probs
  unsolvedProbs: unsolvedProbs
  aojProblemUrl: 'http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id='
  aojUserUrl:    'http://judge.u-aizu.ac.jp/onlinejudge/webservice/user'
  pad0: (str, len) ->
    (new Array(len).join('0') + str).slice(-len)

fs.writeFileSync(outputFilename, html)
console.log html
