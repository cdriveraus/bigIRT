// Person -> response-row index, as compressed sparse rows.
//
// Every kernel groups the responses by person. It used to do so with a
// std::vector<std::vector<int>> built on every call: one heap allocation per
// person and a push_back per response, serially, before the parallel loop,
// and twice per objective evaluation. A counting sort does the same in two
// passes and two allocations, and the rows of person s are then the span
// order[start[s] .. start[s + 1]).
#ifndef BIGIRT_OBSINDEX_H
#define BIGIRT_OBSINDEX_H

#include <vector>
#include <cstddef>

struct BigIRTObsSpan {
  const int* p = nullptr;
  std::size_t n = 0;
  inline std::size_t size() const { return n; }
  inline bool empty() const { return n == 0; }
  inline const int& operator[](const std::size_t i) const { return p[i]; }
  inline const int* begin() const { return p; }
  inline const int* end() const { return p + n; }
};

struct BigIRTObsIndex {
  std::vector<int> order, start;
  // `id` is 1-based person per response. Returns false if an id is out of
  // 1..N, leaving the index empty.
  template <typename IdVec>
  bool build(const IdVec& id, const int Nobs, const int N) {
    start.assign(static_cast<std::size_t>(N) + 1, 0);
    for (int o = 0; o < Nobs; ++o) {
      const int s = id[o] - 1;
      if (s < 0 || s >= N) { start.clear(); order.clear(); return false; }
      ++start[static_cast<std::size_t>(s) + 1];
    }
    for (int s = 0; s < N; ++s) start[static_cast<std::size_t>(s) + 1] += start[static_cast<std::size_t>(s)];
    order.assign(static_cast<std::size_t>(Nobs), 0);
    std::vector<int> fill(start.begin(), start.end() - 1);
    for (int o = 0; o < Nobs; ++o) order[static_cast<std::size_t>(fill[static_cast<std::size_t>(id[o] - 1)]++)] = o;
    return true;
  }
  inline std::size_t size() const { return start.empty() ? 0 : start.size() - 1; }
  inline BigIRTObsSpan operator[](const std::size_t s) const {
    BigIRTObsSpan sp;
    sp.p = order.data() + start[s];
    sp.n = static_cast<std::size_t>(start[s + 1] - start[s]);
    return sp;
  }
};

#endif
