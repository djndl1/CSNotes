#include "avl_tree.h"

#include <stdlib.h>

#define avl_makeTree(n) avl_makeNode(n)

struct Element {
        int dummy;
};

static inline int element_comp(const void *left, const void *right)
{
        const element_t* l = left;
        const element_t* r = right;

        return l->dummy - r->dummy;
}

struct avlNode {
        element_t elem;

        avlTree left;
        avlTree right;
        int height;
};

static inline int avl_height(pNode n)
{
        if (n == NULL)
                return -1;
        else
                return n->height;
}

pNode avl_makeNode(element_t elem)
{
        pNode n = malloc(sizeof(struct avlNode));
        if (n == NULL)
                return NULL;
        n->height = 0;
        n->left = n->right = NULL;
        n->elem = elem;

        return n;
}

static pNode avl_leftSingleRotate(avlTree tree)
{
        pNode newRoot = tree->left;
        tree->left = newRoot->right;
        newRoot->right = tree;

        tree->height = max(avl_height(tree->left), avl_height(tree->right)) + 1;
        newRoot->height = max(avl_height(newRoot->left), avl_height(newRoot->right)) + 1;

        return newRoot;
}

static pNode avl_rightSingleRotate(avlTree tree)
{
        pNode newRoot = tree->right;
        tree->right = newRoot->left;
        newRoot->left = tree;

        tree->height = max(avl_height(tree->left), avl_height(tree->right)) + 1;
        newRoot->height = max(avl_height(newRoot->left), avl_height(newRoot->right)) + 1;

        return newRoot;
}

static pNode avl_leftDoubleRotate(avlTree tree)
{
        tree->left = avl_rightSingleRotate(tree->left);
        return avl_leftSingleRotate(tree);
}

static pNode avl_rightDoubleRotate(avlTree tree)
{
        tree->right = avl_leftSingleRotate(tree->right);
        return avl_rightSingleRotate(tree);
}


avlTree avl_insert(avlTree tree, element_t elem)
{
        if (tree == NULL)
                tree = avl_makeTree(elem);
        else if (element_comp(&elem, &tree->elem) < 0) {
                tree->left = avl_insert(tree->left, elem);
                if (avl_height(tree->left) - avl_height(tree->right) == 2)
                        if (element_comp(&elem, &tree->left->elem) < 0)
                                tree = avl_leftSingleRotate(tree);
                        else
                                tree = avl_leftDoubleRotate(tree);
        } else if (element_comp(&elem, &tree->right->elem) > 0) {
                tree->right = avl_insert(tree->right, elem);
                if (avl_height(tree->right) - avl_height(tree->left) == 2)
                        if (element_comp(&elem, &tree->right->elem) < 0)
                                tree = avl_rightSingleRotate(tree);
                        else
                                tree = avl_rightDoubleRotate(tree);
        }
        tree->height = max(avl_height(tree->left), avl_height(tree->right)) + 1; // important

        return tree;
}
