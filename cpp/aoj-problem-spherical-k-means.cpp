#include <cstdio>
#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <sstream>
#include <random>

#include "data-io.hpp"

// using namespace picojson;
using namespace std;  // danger
#define ALL(c) (c).begin(), (c).end()

// reference_wrapperを使うのは、ポインタなしでクラスタ集合と全体の問題集合を同等に扱うため
typedef reference_wrapper<const Problem> ProblemCRef;

// 密ベクトル
typedef vector<float> DenceVector;

double computeCosine(const Problem& prob, const DenceVector& v) {
  double dot = 0;
  for (auto& desc : prob.descs) dot += v[desc.first] * desc.second;
  return dot;  // assumes |v|=|prob|=1
}

void normalize(DenceVector& v) {
  double norm = sqrt( accumulate(ALL(v), 0.0, [](double a, double b) { return a + b*b; }) );
  transform(ALL(v), v.begin(), [=](double n) { return n/norm; });
}

DenceVector computeMeanDirection(const vector<ProblemCRef>& probs, DenceVector dir) {
  fill(ALL(dir), 0);
  if (probs.empty()) {
    cerr << "undefind direction detected\n";
    dir[0] = 1;
    return dir;
  }
  for (auto& p : probs) {
    for (auto& elm : p.get().descs) dir[elm.first] += elm.second;
  }
  normalize(dir);
  return dir;
}

// 球面k-means法のデータを入れる構造体。
struct SphericalKMeans {
  vector<vector<ProblemCRef>> clusters;
  vector<DenceVector> centroids;
  double quality;
  int numClusters;

  SphericalKMeans(int numClusters = 0, const DenceVector& initialCentroids = DenceVector())
    : clusters(numClusters),
      centroids(numClusters, initialCentroids),
      quality(),
      numClusters(numClusters)
  {}

  void perturbCentroids(int randomSeed) {
    mt19937 randEngine(randomSeed);
    uniform_real_distribution<float> distribNear1(0.8, 1.25);
    for (auto& cv : centroids) {
      transform(ALL(cv), cv.begin(), [&](float n) { return n * distribNear1(randEngine); });
      normalize(cv);
    }
  }

  void computeNewCentroids(const vector<ProblemCRef>& probs) {
    quality = 0;
    for(auto& c : clusters) c.clear();

    for (auto& p : probs) {
      // find a best match centroid
      int bestIdx = -1;
      double bestCosine = -1;
      for (int i = 0; i < numClusters; i++) {
        double angle = computeCosine(p, centroids[i]);
        if (bestCosine < angle) {  // maximize similarity
          bestCosine = angle;
          bestIdx = i;
        }
      }
      clusters[bestIdx].push_back(p);
      quality += bestCosine;
    }

    // update centroids
    for (int i = 0; i < numClusters; i++) {
      centroids[i] = computeMeanDirection(clusters[i], move(centroids[i]));
    }
  }

  void debugPrint() {
    // 見やすくするためコピーしてソート
    SphericalKMeans clusteringData = *this;
    sort(ALL(clusteringData.clusters), [](const vector<ProblemCRef>& a, const vector<ProblemCRef>& b) {
      return a[0].get().id < b[0].get().id;
    });
    for (int i = 0; i < clusteringData.numClusters; i++) {
      auto& cluster = clusteringData.clusters[i];
      printf("cluster#%02d:", i);
      for (auto& p : cluster) printf(" %04d", p.get().id);
      puts("");
    }
  }
};


// 球面k-means法
// http://ibisforest.org/index.php?%E7%90%83%E9%9D%A2%E3%82%AF%E3%83%A9%E3%82%B9%E3%82%BF%E3%83%AA%E3%83%B3%E3%82%B0
// http://www.cs.utexas.edu/users/inderjit/public_papers/concept_mlj.pdf
SphericalKMeans doSphericalKMeans(
    const vector<ProblemCRef>& probs,
    const DenceVector& initialCentroids,
    const int numClusters)
{
  SphericalKMeans bestResult;

  // 複数のシード値でk-meansを行い最善のものを採用する
  for (int randomSeed = 777; randomSeed <= 999; randomSeed++) {
    SphericalKMeans kmeans(numClusters, initialCentroids);
    kmeans.perturbCentroids(randomSeed);

    double prevQuality = -1;
    while (prevQuality < kmeans.quality) {  // 極値にたどり着くまで繰り返しクラスタリングする
      prevQuality = kmeans.quality;
      kmeans.computeNewCentroids(probs);
    }

    if (bestResult.quality < kmeans.quality) bestResult = kmeans;
  }

  return bestResult;
}

// TODO: この関数をどうにかする
vector<vector<vector<int>>> getMsgpackableClustersAndComputeSubclustersIfWanted(const SphericalKMeans& clusteringResult, int dim)
{
  vector<vector<vector<int>>> res(clusteringResult.numClusters, vector<vector<int>>(1));

  for (int i = 0; i < clusteringResult.numClusters; i++) {
    auto& res2 = res[i];
    auto& cluster = clusteringResult.clusters[i];

    if (cluster.size() > 30) {
      // 30を超えるサイズのクラスタは目で識別しづらいのでに再度クラスタリングする。
      // kmeansで階層的にクラスタリングするのは理論的に間違ってるので本来このようなことをするべきでない
      DenceVector meanDirSub = computeMeanDirection(cluster, DenceVector(dim));
      int numSubClusters = 2 + cluster.size() / 60;  // てきとうにサイズをでっちあげる
      SphericalKMeans prop = doSphericalKMeans(cluster, meanDirSub, numSubClusters);
      res2.resize(numSubClusters);
      for (int j = 0; j < numSubClusters; j++) {
        for (auto& p : prop.clusters[j]) res2[j].push_back(p.get().id);
      }

    } else {
      for (auto& p : cluster) res2[0].push_back(p.get().id);
    }
  }
  return res;
}

int main(int argc, char* argv[]) {

  if (argc < 2) return 1;
  vector<Problem> problems = readProblemData(argv[1]);

  // normalize feature vectors in advance
  // TODO: calc idf and use tf-idf
  for (auto& p : problems) {
    double norm = sqrt( accumulate(p.descs.begin(), p.descs.end(), 0LL,
        [](long long a, std::pair<int, int> b) { return a + (long long)b.second*b.second; }) );
    for (auto& desc : p.descs) desc.second /= norm;
  }

  // gather some more info
  int dim = max_element(ALL(problems), [](Problem& a, Problem& b) {
                                         return a.descs.back().first < b.descs.back().first;
                                       })->descs.back().first + 1;
  vector<ProblemCRef> problemRefs(ALL(problems));
  DenceVector meanDir = computeMeanDirection(problemRefs, DenceVector(dim));

  int numClustersList[] = {50,60,70};
  for (int numClusters : numClustersList)
  {
    // do clustering
    SphericalKMeans clusteringResult = doSphericalKMeans(problemRefs, meanDir, numClusters);

    // print debug info
    cout << numClusters << " " << clusteringResult.quality << endl;
    clusteringResult.debugPrint();

    // write clusters to file
    auto msgpackCls = getMsgpackableClustersAndComputeSubclustersIfWanted(clusteringResult, dim);
    stringstream fnameStream;
    fnameStream << "../clusters-" << numClusters << ".msg";  // TODO: make fnames specifiable somewhere
    writeClusters(fnameStream.str().c_str(), msgpackCls);
  }

  return 0;
}
