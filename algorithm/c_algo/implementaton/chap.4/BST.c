#include "tree.h"

#include <stdbool.h>
#include <stdlib.h>

#define bst_nodeDestroy(n) free(n)

struct Element {
        int dummy;
};

struct BinaryTreeNode {
        element_t element;
        pBTNode left;
        pBTNode right;
};

static inline pBTNode bst_makeNode(element_t elm)
{
        pBTNode n = malloc(sizeof(struct BinaryTreeNode));
        if (n == NULL)
                return NULL;
        n->left = n->right = NULL;
        n->element = elm;

        return n;
}

static inline int element_comp(void *left, void *right)
{
        element_t* l = left;
        element_t* r = right;

        return l->dummy - r->dummy;
}

BST bst_makeEmpty(BST tree)
{
        if ( tree != NULL) {
                bst_makeEmpty(tree->left);
                bst_makeEmpty(tree->right);
                bst_nodeDestroy(tree);
        }
        return NULL;
}

pBTNode bst_find(BST tree, element_t elm)
{
        if ( tree == NULL)
                return NULL;
        // actually tail recursion
        if (element_comp(&tree->element, &elm) < 0)
                return bst_find(tree->right, elm);
        else if (element_comp(&tree->element, &elm) > 0)
                return bst_find(tree->left, elm);
        else
                return tree;
}

pBTNode bst_findMin(BST tree)
{
        if (tree == NULL)
                return NULL;
        while (tree->left != NULL)
                tree = tree->left;
        return tree;
}

pBTNode bst_findMax(BST tree)
{
        if (tree == NULL)
                return NULL;
        while (tree->right != NULL) {
                tree = tree->right;
        }
        return tree;
}

BST bst_insert(BST tree, element_t elm)
{
        if (tree == NULL) {
                tree =  bst_makeNode(elm);
        } else if (element_comp(&tree->element, &elm) < 0) {
                BST sub = bst_insert(tree->right, elm);
                tree->right = sub;
        } else if (element_comp(&tree->element, &elm) > 0) {
                BST sub = bst_insert(tree->left, elm);
                tree->left = sub;
        }
        return tree;
}

element_t bst_deleteMin(BST tree)
{
        pBTNode parent = tree;
        while (tree->left != NULL) {
                parent = tree;
                tree = tree->left;
        }
        if (tree->right != NULL)
                parent->left = tree->right;
        element_t tmp = tree->element;
        bst_nodeDestroy(tree);

        return tmp;
}

BST bst_delete(BST tree, element_t elm)
{
        pBTNode tmp;

        if (tree == NULL)
                return NULL;
        else if (element_comp(&tree->element, &elm) > 0)
                tree->left = bst_delete(tree->left, elm);
        else if (element_comp(&tree->element, &elm) < 0)
                tree->right = bst_delete(tree->right, elm);
        else {
                if (tree->left && tree->right) {
                        tree->element = bst_deleteMin(tree->right);
                } else {
                        tmp = tree;
                        // also handles zero child
                        if (tree->left == NULL)
                                tree = tree->right;
                        else if (tree->right == NULL)
                                tree = tree->left;
                        bst_nodeDestroy(tmp);
                }
        }

        return tree;
}
