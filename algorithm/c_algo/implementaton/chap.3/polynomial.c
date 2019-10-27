/* When designing a ctor-like interface that may allocate some memory,
 * return the pointer instead of returning by argument, in which
 * case the outer variable will not be initialized, causing memory
 * leak.
 */


#include <stdlib.h>
#include <stdio.h>

#include "../lib/xmalloc.h"

typedef struct Term *pTerm;

struct Term {
        int Coefficient;
        int Exponent;
        pTerm Next;
};

typedef pTerm Polynomial;

/**
 * Precondition
 *      coef: coefficient of the new term
 *      exp: exponent of the new term
 * Postcondtion
 *      a new term is constructed on the heap
 * returns: the newly-constructed term pointing to `NULL`
 */
pTerm makeTerm(int coef, int exp)
{
        pTerm tmp;
        tmp = xmalloc(sizeof(struct Term));
        tmp->Coefficient = coef;
        tmp->Exponent = exp;
        tmp->Next = NULL;

        return tmp;
}


/**
 * Precondition:
 * Postcondtion:
 *      - poly: polynomial with a zero term 
 * return: an initialized polynomial with a zero term, or `NULL` if failed
 */
Polynomial ZeroPolynomial(void)
{
        Polynomial poly = makeTerm(0, 0);
        pTerm zeroTerm = makeTerm(0, 0);

        poly->Next = zeroTerm;

        return poly;
}

/**
 * Precondition:
 *      - poly: an initialized polynomial
 *      - exp: the exponent
 * Postcondtion:
 * return: the term whose exponent is greater than exp
 */
pTerm findPosition(const Polynomial poly, int exp)
{
        pTerm tmpTerm = poly->Next;
        pTerm prev = poly;
        while (tmpTerm != NULL) {
                if (tmpTerm->Exponent > exp) {
                        prev = tmpTerm;
                        tmpTerm = tmpTerm->Next;
                } else
                        break;
        }
        return prev;
}

/**
 * Precondition:
 *      - poly: an initialized polynomial
 *      - exp: exponent
 *      - coef: coefficient
 * Postcondtion:
 *      a term of `exp` degrees and with `coef` as the coefficient
 * is added to `poly`
 * returns: 0 success; -1 bad allocation
 */
int addTerm(Polynomial poly, int exp, int coef)
{
        pTerm pos = findPosition(poly, exp);
        if (pos->Next == NULL) {
                pTerm tmp = makeTerm(coef, exp);
                pos->Next = tmp;
                return 0;
        }

        if (pos->Next->Exponent == exp) {
                pos->Next->Coefficient += coef;
        } else {
                pTerm tmp = makeTerm(coef, exp);
                tmp->Next = pos->Next;
                pos->Next = tmp;
        }

        return 0;
}

/*
 * Precondition:
 *      - term: a term or a NULL pointer
 * Postcondition:
 *      - term and all its successors are destroyed; does nothing if `term` is NULL
 */
void deleteSubPolynomial(pTerm term)
{
        if (term == NULL)
                return;
        pTerm P = term->Next;
        free(term);
        while (P != NULL) {
                pTerm tmp = P;
                P = P->Next;
                free(tmp);
        }
}

/**
 * Precondition:
 *      - old: an initialized polynomial
 *      - new: an uninitialized polynomial
 * Postcondition:
 *      - new: is constructed identical to `old`
 * returns: a duplicate of `old` if success; otherwise `NULL`
 */
Polynomial dupPolynomial(const Polynomial old)
{
        Polynomial new = ZeroPolynomial();

        pTerm nTerm = new->Next;
        pTerm oTerm = old->Next;
        nTerm->Coefficient = oTerm->Coefficient;
        nTerm->Exponent = oTerm->Exponent;
        if (oTerm->Next == NULL)
                return new;

        oTerm = oTerm->Next;
        while (oTerm != NULL) {
                pTerm newTerm = makeTerm(oTerm->Coefficient, oTerm->Exponent);
                nTerm->Next = newTerm;
                nTerm = nTerm->Next;
                oTerm = oTerm->Next;
        }
        return new;
}


/**
 * Precondition:
 *      - first, second: initialized polynomials
 *      - result: uninitialized polynomials
 * Postcondition:
 *      - result is the sum of `first` and `second`
 * returns: the sum of `first` and `second` if success; `NULL` otherwise
 */
Polynomial addPolynomial(const Polynomial first,const Polynomial second)
{
        Polynomial result;
        result = ZeroPolynomial();
        result = dupPolynomial(first);

        pTerm tmp = second->Next;
        while (tmp != NULL) {
                addTerm(result, tmp->Exponent, tmp->Coefficient);
                tmp = tmp->Next;
        }
        return result;
}


Polynomial multiplyPolynomial(const Polynomial first, const Polynomial second)
{
        Polynomial result;
        result = ZeroPolynomial();

        for (pTerm ftmp = first->Next; ftmp != NULL; ftmp = ftmp->Next) {
                for (pTerm stmp = second->Next; stmp != NULL; stmp = stmp->Next) {
                        int coef = ftmp->Coefficient * stmp->Coefficient;
                        int exp = ftmp->Exponent + stmp->Exponent;
                        addTerm(result, exp, coef);
                }
        }
        return result;
}

void printPolynomial(const Polynomial poly)
{
        pTerm tmp;
        for (tmp = poly->Next; tmp->Next != NULL; tmp = tmp->Next) {
                printf(" %dX^%d ", tmp->Coefficient, tmp->Exponent);
                putchar('+');
        }
        printf(" %dX^%d\n", tmp->Coefficient, tmp->Exponent);
}

int main(int argc, char *argv[])
{
        Polynomial p1, p2;
        p1 = ZeroPolynomial();
        addTerm(p1, 1000, 10);
        addTerm(p1, 14, 5);
        addTerm(p1, 0, 1);
        printPolynomial(p1);

        p2 = ZeroPolynomial();
        addTerm(p2, 1990, 3);
        addTerm(p2, 1492, -2);
        addTerm(p2, 1, 11);
        addTerm(p2, 0, 5);
        printPolynomial(p2);

        Polynomial sum = addPolynomial(p1, p2);
        printPolynomial(sum);
        Polynomial prod = multiplyPolynomial(p1, p2);
        printPolynomial(prod);
        return 0;
}

