fs      = require 'fs'
msgpack = require 'msgpack'
jade    = require 'jade'
less    = require 'less'

if process.argv.length < 4
  console.log "Usage: coffee prog clusters-msg-file out-html-file"
  return 1

clustersFilename = process.argv[2]
outputFilename = process.argv[3]
TEMPLATE_FILENAME = "template-hclust.jade"

pad0 = (str, len) ->
  (new Array(len).join('0') + str).slice(-len)

fs.readFile clustersFilename, (err, data) ->
  if err then throw err
  clusters = ((clusters, novelty) ->
    me = name: "", children: [], novelty: 0 , height: 0
    if clusters instanceof Array
      for cluster, i in clusters   # use unfancy for loop to avoid stack overflow
        child = arguments.callee cluster, if i is 1 then 0 else novelty + 1
        me.children.push child
        # me.height = Math.max(me.height, child.height + 1)
        # 真の高さ情報はRからエクスポートしたときにネグってしまったのでは？
    else
      me = name: pad0(clusters, 4), novelty: +novelty  # should be height
    me
  )(msgpack.unpack data, 0)
  # console.log JSON.stringify clusters, null, 2

  parser = jade.compile(fs.readFileSync(TEMPLATE_FILENAME), pretty: no)
  html = parser
    clusters: clusters
    clustersStr: JSON.stringify clusters
    aojProblemUrl: 'http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id='
    aojUserUrl:    'http://judge.u-aizu.ac.jp/onlinejudge/webservice/user'
    pad0: pad0
  fs.writeFileSync(outputFilename, html)
  # console.log html
