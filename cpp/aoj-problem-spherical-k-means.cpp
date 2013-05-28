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
typedef vector<double> DenceVector;

double computeCosine(const Problem& prob, const DenceVector& v) {
  double dot = 0;
  for (auto& desc : prob.descs) dot += v[desc.first] * desc.second;
  return dot / prob.norm;
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
  for (auto p : probs) {
    for (auto& elm : p.get().descs) {
      dir[elm.first] += elm.second / p.get().norm;
      // 毎回割り算しているが内積計算の方が多いのでおそらくこの方が速い
    }
  }
  normalize(dir);
  return dir;
}

// クラスタリングの構造体。
// TODO: Refactor. it lacks scalability. the way to store best result is strange
struct SphericalKMeans {
  vector<vector<ProblemCRef>> clusters;
  vector<DenceVector> conceptVectors;
  double quality;
  int numClusters;

  SphericalKMeans() : quality() {}

  SphericalKMeans(int numClusters, const DenceVector& initialConceptVec, int randomSeed)
    : clusters(numClusters),
      conceptVectors(numClusters, initialConceptVec),
      quality(),
      numClusters(numClusters)
  {
    // obtain initial concept vectors by perturbing
    mt19937 randEngine(randomSeed);
    uniform_real_distribution<double> distribNear1(0.8, 1.25);
    for (auto& cv : conceptVectors) {
      transform(ALL(cv), cv.begin(), [&](double n) { return n * distribNear1(randEngine); });
      normalize(cv);
    }
  }

  // 極値にたどり着くまでクラスタリングを実行
  void compute(const vector<ProblemCRef>& probs)
  {
    double prevQuality = -1;
    while (prevQuality < quality) {
      prevQuality = quality;
      quality = 0;
      for(auto& c : clusters) c.clear();

      for (auto& p : probs) {
        // find a best match concept vector
        int bestIdx = -1;
        double bestAngle = -1;
        for (int i = 0; i < numClusters; i++) {
          double angle = computeCosine(p, conceptVectors[i]);
          if (bestAngle < angle) {  // angle is cosine, so we should maximize it
            bestAngle = angle;
            bestIdx = i;
          }
        }
        clusters[bestIdx].push_back(p);
        quality += bestAngle;
      }

      // update concept vectors
      for (int i = 0; i < numClusters; i++) {
        conceptVectors[i] = computeMeanDirection(clusters[i], move(conceptVectors[i]));
      }
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
    const DenceVector& initialConceptVec,
    const int numClusters)
{
  SphericalKMeans bestResult;

  for (int randomSeed = 777; randomSeed <= 999; randomSeed++) {
    SphericalKMeans prop(numClusters, initialConceptVec, randomSeed);
    prop.compute(probs);

    if (bestResult.quality < prop.quality) bestResult = prop;
  }

  return bestResult;
}

// msgpackで吐くための構造体、いらないか
// struct SphericalKMeansCluster {
//   vector<int> problemIds;
//   // DenceVector conceptVectors;
//   double quality;
//   MSGPACK_DEFINE(problemIds, quality);
// };

// TODO: この関数をどうにかする
vector<vector<vector<int>>> getMsgpackableClustersAndComputeSubclustersIfWanted(const SphericalKMeans& clusteringResult, int dim)
{
  vector<vector<vector<int>>> res(clusteringResult.numClusters, vector<vector<int>>(1));

  for (int i = 0; i < clusteringResult.numClusters; i++) {
    auto& res2 = res[i];
    auto& cluster = clusteringResult.clusters[i];

    if (cluster.size() > 30) {  // この分岐最高にアホ
      // printf("Size of cluster#%02d is as too big as %d! Lets have a hierarchal clustering\n", i, cluster.size());
      // kmeansで作ったクラスタに対し階層的にクラスタリングするのは理論的に間違ってるが、
      // クラスタが小さい方が目に見やすいのでに再度クラスタリングする（酷い）
      // 元からまともにクラスタリングできてないから大して問題ないさ
      DenceVector meanDirSub = computeMeanDirection(cluster, DenceVector(dim));
      int numSubClusters = 2 + cluster.size() / 60;
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

  // gather some more info
  int dim = max_element(ALL(problems), [](Problem& a, Problem& b) {
                                         return a.descs.back().first < b.descs.back().first;
                                       })->descs.back().first + 1;
  vector<ProblemCRef> problemRefs(ALL(problems));
  DenceVector meanDir = computeMeanDirection(problemRefs, DenceVector(dim));

  // int maxNumClusters = 60;
  int numClustersList[] = {50,60,70};
  for (int numClusters : numClustersList)
  {
    SphericalKMeans clusteringResult = doSphericalKMeans(problemRefs, meanDir, numClusters);
    cout << numClusters << " " << clusteringResult.quality << endl;

    // show them!
    // if (numClusters == maxNumClusters) {
      clusteringResult.debugPrint();
      auto msgpackCls = getMsgpackableClustersAndComputeSubclustersIfWanted(clusteringResult, dim);
      stringstream fnameStream;
      fnameStream << "../clusters-" << numClusters << ".msg";  // TODO: make fnames specifiable somewhere
      writeClusters(fnameStream.str().c_str(), msgpackCls);
    // }
  }

  return 0;
}
