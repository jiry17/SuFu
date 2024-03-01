//
// Created by pro on 2021/12/5.
//

#ifndef ISTOOL_BITSET_H
#define ISTOOL_BITSET_H

#include <vector>
#include <string>

class Bitset {
    std::vector<unsigned int> A;
    unsigned int n;
public:
    std::string toString() const;
    std::string toXString() const;
    Bitset(std::vector<unsigned int>&& _A, unsigned int _n): A(std::move(_A)), n(_n) {}
    Bitset(): n(0) {}
    Bitset(unsigned int n, bool c);
    int count() const;
    int size() const {return int(n);}
    int getASize() const {return A.size();}
    unsigned int accessA(int pos) const {return A[pos];}
    void append(unsigned int k);
    void set(unsigned int pos, unsigned int w);
    Bitset operator | (const Bitset& x) const;
    Bitset operator & (const Bitset& x) const;
    Bitset operator ^ (const Bitset& x) const;
    Bitset operator ~ () const;
    Bitset exclude (const Bitset& x) const;
    bool checkCover(const Bitset& x) const;
    bool operator [] (unsigned int k) const;
    bool operator < (const Bitset& x) const;
    bool operator == (const Bitset& x) const;
};


#endif //ISTOOL_BITSET_H
