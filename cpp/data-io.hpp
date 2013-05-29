#include <vector>
#include <utility>

// 問題を表すクラス。
// 処理内容はAOJの問題に依存しないので、名前を変えたほうがよさげ。
struct Problem {
  // AOJでの問題番号
  int id;
  // 特徴ベクトル。疎ベクトルである。
  std::vector<std::pair<int, float>> descs;
  // 特徴ベクトルの長さ。球面k-meansではベクトルを長さ1に正規化して使う。
  double norm;
};

std::vector<Problem> readProblemData(const char* msgFilename);

bool writeClusters(const char* msgFilename, const std::vector<std::vector<std::vector<int>>>& clusters);
