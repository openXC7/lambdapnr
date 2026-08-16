// Eigen CG shim for lambdapnr's HeAP placer.
//
// Mirrors the exact solver call made by nextpnr-0.10's placer_heap:
//   Eigen::ConjugateGradient<SparseMatrix<double>, Lower | Upper> solver;
//   solver.setTolerance(tolerance);
//   VectorXd xr = solver.compute(mat).solveWithGuess(vb, vx);
// with mat built column by column. Duplicate (row, col) entries are
// summed by Eigen (matching mat.insert(...) += semantics); the caller
// normally passes already-accumulated unique entries.
//
// Compiled with -O3 -DNDEBUG against the same Eigen headers (3.5.0,
// /usr/include/eigen3) as the reference nextpnr build, so the CG loop
// and its packetized reductions are bit-identical.

#include <Eigen/IterativeLinearSolvers>
#include <Eigen/Sparse>
#include <algorithm>
#include <vector>

extern "C" int lp_solve_cg(int n, const int *colptr, const int *rowidx, const double *vals, const double *rhs,
                           const double *guess, double tol, double *out)
{
    Eigen::SparseMatrix<double> mat(n, n);
    // Build the matrix exactly as nextpnr's EquationSystem::solve does:
    // reserve per-column nnz, then insert in (col, row) sorted order.
    // setFromTriplets produced a subtly different internal layout that made
    // the CG matrix-vector reductions sum in a different order, drifting the
    // solution at ~1e-9 -- enough to flip a spread cut boundary downstream.
    std::vector<int> colnnz;
    for (int col = 0; col < n; col++)
        colnnz.push_back(colptr[col + 1] - colptr[col]);
    mat.reserve(colnnz);
    for (int col = 0; col < n; col++)
        for (int i = colptr[col]; i < colptr[col + 1]; i++)
            mat.insert(rowidx[i], col) = vals[i];

    Eigen::VectorXd vx(n), vb(n);
    for (int i = 0; i < n; i++) {
        vx[i] = guess[i];
        vb[i] = rhs[i];
    }

    Eigen::ConjugateGradient<Eigen::SparseMatrix<double>, Eigen::Lower | Eigen::Upper> solver;
    solver.setTolerance(tol);
    Eigen::VectorXd xr = solver.compute(mat).solveWithGuess(vb, vx);
    for (int i = 0; i < n; i++)
        out[i] = xr[i];
    return 0;
}

// Sort indices [0..n) by the raw double at each index, using std::sort
// EXACTLY like nextpnr's cut_region does (unstable introsort). The Haskell
// sortBy is stable, so ties (equal raw positions) come out in a different
// order than the C++ std::sort; reordering here reproduces the C++ tie order
// bit-for-bit.
extern "C" void lp_sort_indices(int n, const double *raw, int *out)
{
    std::vector<int> idx(n);
    for (int i = 0; i < n; i++)
        idx[i] = i;
    std::sort(idx.begin(), idx.end(), [&](int a, int b) { return raw[a] < raw[b]; });
    for (int i = 0; i < n; i++)
        out[i] = idx[i];
}
