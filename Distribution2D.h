#ifndef DISTRIBUTION_2D_H
#define DISTRIBUTION_2D_H

#include <vector>
#include <algorithm>

namespace aether {

// Taken from https://github.com/mmp/pbrt-v3/blob/master/src/core/sampling.h
// and https://github.com/mmp/pbrt-v3/blob/master/src/core/pbrt.h

template <typename T, typename U, typename V>
inline T Clamp(T val, U low, V high) {
    if (val < low) {
        return low;
    } else if (val > high) {
        return high;
    } else {
        return val;
    }
}

template <typename Predicate>
int FindInterval(int size, const Predicate &pred) {
    int first = 0, len = size;
    while (len > 0) {
        int half = len >> 1, middle = first + half;
        // Bisect range based on value of _pred_ at _middle_
        if (pred(middle)) {
            first = middle + 1;
            len -= half + 1;
        } else
            len = half;
    }
    return Clamp(first - 1, 0, size - 2);
}

struct Distribution1D {
    Distribution1D(const Real *f, int n) : func(f, f + n), cdf(n + 1) {
        cdf[0] = 0.;
        for (int i = 1; i < n + 1; ++i) {
            cdf[i] = cdf[i - 1] + func[i - 1] / n;
        }

        funcInt = cdf[n];
        if (funcInt == 0.f) {
            for (int i = 1; i < n + 1; ++i) {
                cdf[i] = Real(i) / Real(n);
            }
        } else {
            for (int i = 1; i < n + 1; ++i) {
                cdf[i] /= funcInt;
            }
        }
    }
    inline int Count() const { return func.size(); }
    Real Sample(const Real u, int *off = nullptr) const {
        int offset = FindInterval(cdf.size(),
                                  [&](int index) { return cdf[index] <= u; });
        if (off) {
            *off = offset;
        }

        Real du = u - cdf[offset];
        if ((cdf[offset + 1] - cdf[offset]) > 0) {
            du /= (cdf[offset + 1] - cdf[offset]);
        }
        return (offset + du) / Count();
    }

    std::vector<Real> func, cdf;
    Real funcInt;
};

struct Distribution2D {
    Distribution2D(const Real *f, int nu, int nv) {
	    pConditionalV.reserve(nv);
	    for (int v = 0; v < nv; ++v) {
	        pConditionalV.emplace_back(new Distribution1D(&f[v * nu], nu));
	    }
	    // Compute marginal sampling distribution $p[\tilde{v}]$
	    std::vector<Real> marginalFunc;
	    marginalFunc.reserve(nv);
	    for (int v = 0; v < nv; ++v) {
	        marginalFunc.push_back(pConditionalV[v]->funcInt);
	    }
	    pMarginal.reset(new Distribution1D(&marginalFunc[0], nv));
    }
    std::array<Real, 2> Sample(const std::array<Real, 2> &uv) const {
        int v;
        const Real d1 = pMarginal->Sample(uv[1], &v);
        const Real d0 = pConditionalV[v]->Sample(uv[0]);
        return std::array<Real, 2>{{d0, d1}};
    }
    Real Pdf(const std::array<Real, 2> &p) const {
        int iu = std::min(std::max(int(p[0] * pConditionalV[0]->Count()), 0), pConditionalV[0]->Count() - 1);
        int iv = std::min(std::max(int(p[1] * pMarginal->Count()), 0), pMarginal->Count() - 1);
        return pConditionalV[iv]->func[iu] / pMarginal->funcInt;
    }

    std::vector<std::unique_ptr<Distribution1D>> pConditionalV;
    std::unique_ptr<Distribution1D> pMarginal;
};

} // namespace aether

#endif // DISTRIBUTION_2D_H
