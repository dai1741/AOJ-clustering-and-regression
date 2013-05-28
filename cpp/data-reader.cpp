#include <string>
#include <iostream>
#include <fstream>
#include <iterator>
#include <map>

// 疎ベクトルはstd::vectorで実装するのが手軽らしい: http://research.preferred.jp/2011/11/sparse-vector/
// #include <boost/numeric/ublas/vector.hpp>
#include <msgpack.hpp>

#include "data-reader.hpp"

std::vector<Problem> readProblemData(const char* msgFilename) {
  std::ifstream file(msgFilename, std::ios::binary);
  if (not file.is_open()) exit(2);

  std::vector<char> byteData((std::istreambuf_iterator<char>(file)),
                              std::istreambuf_iterator<char>());
  msgpack::unpacked msg;
  msgpack::unpack(&msg, &byteData[0], byteData.size());
  std::vector<std::map<std::string, msgpack::object>> jsonData;
  msg.get().convert(&jsonData);

  std::vector<Problem> probs(jsonData.size());
  transform(jsonData.begin(), jsonData.end(), probs.begin(),
      [](decltype(jsonData[0])& v) {
        Problem p;
        std::vector<int> solvers, codeSizes;
        v["problemId"].convert(&p.id);
        v["solvers"  ].convert(&solvers);
        v["codeSizes"].convert(&codeSizes);
        for (auto sIt = solvers.begin(), cIt = codeSizes.begin(); sIt != solvers.end(); ) {
          p.descs.emplace_back(*sIt++, *cIt++);
          // p.descs.back().second = 1;
        }
        p.norm = sqrt( accumulate(p.descs.begin(), p.descs.end(), 0LL,
            [](long long a, std::pair<int, int> b) { return a + (long long)b.second*b.second; }) );
        // p.norm = 1;
        return p;
      });
  return probs;
}

bool writeClusters(const char* msgFilename, const std::vector<std::vector<std::vector<int>>>& clusters) {
  msgpack::sbuffer buffer;
  msgpack::pack(&buffer, clusters);
  std::ofstream out(msgFilename, std::ios::binary);
  out.write(buffer.data(), buffer.size());
  return true;  // im too lazy to check
}
