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
#include <vector>

extern "C" int lp_solve_cg(int n, const int *colptr, const int *rowidx, const double *vals, const double *rhs,
                           const double *guess, double tol, double *out)
{
    Eigen::SparseMatrix<double> mat(n, n);
    std::vector<Eigen::Triplet<double>> trips;
    for (int col = 0; col < n; col++)
        for (int i = colptr[col]; i < colptr[col + 1]; i++)
            trips.emplace_back(rowidx[i], col, vals[i]);
    mat.setFromTriplets(trips.begin(), trips.end());

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
