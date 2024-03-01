//
// Created by pro on 2020/12/10.
//

#include "istool/basic/bitset.h"

#include <cassert>
#include <iostream>

void Bitset::append(unsigned int k) {
#ifdef DEBUG
    assert(k == 0 || k == 1);
#endif
    if ((n & 31u) == 0) A.push_back(k);
    else if (k) A[n >> 5u] |= (1u << (n & 31u));
    ++n;
}

void Bitset::set(unsigned int pos, unsigned int w) {
#ifdef DEBUG
    assert(w == 0 || w == 1);
#endif
    if (((A[pos >> 5u] >> (pos & 31u)) & 1u) != w)
        A[pos >> 5u] ^= (1u << (pos & 31u));
}

int Bitset::count() const {
    int ans = 0;
    for (auto& w: A) ans += __builtin_popcount(w);
    return ans;
}

Bitset Bitset::operator&(const Bitset &x) const{
    std::vector<unsigned int> result(A);
#ifdef DEBUG
    assert(x.n == n);
#endif
    for (int i = 0; i < A.size(); ++i) {
        result[i] &= x.A[i];
    }
    return Bitset(std::move(result), n);
}

Bitset Bitset::operator|(const Bitset &x) const{
    std::vector<unsigned int> result(A);
#ifdef DEBUG
    assert(x.n == n);
#endif
    for (int i = 0; i < A.size(); ++i) {
        result[i] |= x.A[i];
    }
    return Bitset(std::move(result), n);
}

Bitset Bitset::operator^(const Bitset &x) const {
    std::vector<unsigned int> result(A);
#ifdef DEBUG
    assert(x.n == n);
#endif
    for (int i = 0; i < A.size(); ++i) {
        result[i] ^= x.A[i];
    }
    return Bitset(std::move(result), n);
}

Bitset Bitset::exclude(const Bitset &x) const {
    std::vector<unsigned int> result(A);
#ifdef DEBUG
    assert(x.n ==n);
#endif
    for (int i = 0; i < A.size(); ++i) {
        result[i] = result[i] & (~x.A[i]);
    }
    return Bitset(std::move(result), n);
}

bool Bitset::checkCover(const Bitset &x) const {
#ifdef DEBUG
    assert(x.n == n);
#endif
    // std::cout << "cover " << A.size() << " " << x.A.size() << std::endl;
    for (int i = 0; i < A.size(); ++i) {
        if ((A[i] & x.A[i]) != x.A[i]) return false;
    }
    return true;
}

std::string Bitset::toString() const {
    std::string result = "";
    for (int i = 0; i < n; ++i) result += std::to_string((*this)[i]);
    return result;
}

std::string Bitset::toXString() const {
    if (n == 0) return "#x0";
    std::string result = "#x";
    std::vector<char> A;
    for (int i = 0; i <= n - 1; i += 4) {
        int w = 0;
        for (int j = 0; j < 4 && i + j < n; ++j) {
            w += ((*this)[i + j] << j);
        }
        if (w < 10) A.push_back(w + '0'); else A.push_back(w - 10 + 'a');
    }
    for (int i = A.size(); i; --i) result += A[i - 1];
    return result;
}

bool Bitset::operator[](unsigned int k) const {
    return (A[k >> 5u] >> (k & 31u)) & 1u;
}

Bitset::Bitset(unsigned int _n, bool c): n(_n) {
    unsigned int m = n >> 5u;
    A.resize(m);
    for (int i = 0; i < m; ++i) {
        A[i] = c ? -1 : 0;
    }
    if (n & 31u) {
        if (c) A.push_back((1u << (n & 31u)) - 1); else A.push_back(0);
    }
}

Bitset Bitset::operator~() const {
    std::vector<unsigned int> result(A);
    for (int i = 0; i < A.size(); ++i) {
        result[i] = ~result[i];
    }
    if (n & 31) {
        int last = result.size() - 1;
        unsigned int rem = n & 31;
        result[last] &= ((1u << rem) - 1);
    }
    return Bitset(std::move(result), n);
}

bool Bitset::operator < (const Bitset& x) const {
    if (n < x.n) return true; else if (n > x.n) return false;
    return A < x.A;
}

bool Bitset::operator == (const Bitset &x) const {
    if (n != x.n) return false;
    for (int i = 0; i < A.size(); ++i) {
        if (A[i] != x.A[i]) return false;
    }
    return true;
}