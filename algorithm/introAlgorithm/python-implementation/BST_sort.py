#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import operator

class TreeNode:
    def __init__(self, key):
        self.key = key
        self.parent = None
        self.left = None
        self.right = None

    def subtree_leftmost_iterative(self):
        n = self
        while n.left:
            n = n.left
        return n

    def subtree_leftmost_recursive(self):
        if self.left:
            return self.left.subtree_leftmost_recursive()
        else:
            return self

    def subtree_rightmost_iterative(self):
        n = self
        while n.right:
            n = n.right
        return n

    def subtree_right_recursive(self):
        if self.right:
            return self.right.subtree_rightmost_recursive()
        else:
            return self

    def subtree_rightmost_iterative(self):
        n = self
        while n.right:
            n = n.right
        return n

    def subtree_search_recursive(self, key, comp):
        if self.key == key:
            return self
        if comp(key, self.key):
            return self.left.subtree_search_recursive(key, comp)
        else:
            return self.right.subtree_search_recursive(key, comp)

    def subtree_search_iterative(self, key, comp):
        tmp = self
        while tmp and tmp.key != key:
            if comp(key, self.key):
                tmp = tmp.left
            else:
                tmp = tmp.right
        return tmp

    def successor(self):
        if self.right:
            return self.right.subtree_leftmost_iterative()
        x = self
        y = x.parent
        while y and y.right == x:
            x = y
            y = y.parent
        return y

    def predecessor(self):
        if self.left:
            return self.left.subtree_rightmost_iterative()
        x = self
        y = x.parent
        while y and y.left == x:
            x = y
            y = y.parent
        return y

    def insert_iterative(self, n, comp):
        y = None
        x = self
        while x:
            y = x
            if comp(n.key, x.key):
                x = x.left
            else:
                x = x.right
        n.parent = y

        if comp(n.key, y.key):
            y.left = n
        else:
            y.right = n

    def insert_recursive(self, n, comp):
        if comp(n.key, self.key):
            if self.left:
                self.left.insert_recursive(n, comp)
            else:
                self.left = n
        else:
            if self.right:
                self.right.insert_recursive(n, comp)
            else:
                self.right = n

    @staticmethod
    def subtree_transplant(old, new):
        if old == old.parent.left:
            old.parent.left = new
        else:
            old.parent.right = new
        if new:
            new.parent = old.parent

    def delete(self, n):
        if not n:
            return
        if not n.left:
            TreeNode.subtree_transplant(n, n.right)
        if not n.right:
            TreeNode.subtree_transplant(n, n.left)
        else:
            y = n.right.subtree_leftmost_iterative()
            if y.parent != n:
                TreeNode.subtree_transplant(y, y.right)
                y.right = n.right
                y.right.parent = y
            # now y is the root of n's right tree
            TreeNode.subtree_transplant(n, y)
            y.left = n.left
            y.left.parent = y

    def subtree_inorder_traversal_recursive(self):
        if self.left:
            lt = self.left.subtree_inorder_traversal_recursive()
        else:
            lt = []
        lt.append(self.key)
        if self.right:
            rt = self.right.subtree_inorder_traversal_recursive()
            lt.extend(rt)
        return lt

    def subtree_inorder_traversal_iterative(self):
        stack = []
        curr = self

        keys = []
        # invariant: every popped node is the successor
        # of the previous popped node
        while curr or stack:
            # always gets the leftmost line
            while curr:
                stack.append(curr)
                curr = curr.left
            curr = stack.pop()
            keys.append(curr.key)
            # time to turn right
            curr = curr.right
        return keys

    def subtree_preorder_traversal_recursive(self):
        keys = []
        keys.append(self.key)
        if self.left:
            lt = self.left.subtree_preorder_traversal_recursive()
            keys.extend(lt)
        if self.right:
            rt = self.right.subtree_preorder_traversal_recursive()
            keys.extend(rt)
        return keys

    def subtree_preorder_traversal_iterative(self):
        stack = []
        keys = []
        stack.append(self)

        while stack:
            curr = stack.pop()
            keys.append(curr.key)
            if curr.right:
                stack.append(curr.right)
            if curr.left:
                stack.append(curr.left)

        return keys

    def subtree_postorder_traversal_recursive(self):
        keys = []
        if self.left:
            lt = self.left.subtree_postorder_traversal_recursive()
            keys.extend(lt)
        if self.right:
            rt = self.right.subtree_postorder_traversal_recursive()
            keys.extend(rt)
        keys.append(self.key)

        return keys

    def subtree_postorder_traversal_iterative(self):
        keys = []
        curr = self
        stack = []
        stack.append(curr)
        pass

    def subtree_getlevel_recursive(self, level):
        if level == 1:
            keys = []
            keys.append(self.key)
            return keys

        level_list = []
        if self.left:
            lt = self.left.subtree_getlevel_recursive(level-1)
            level_list.extend(lt)
        if self.right:
            rt = self.right.subtree_getlevel_recursive(level-1)
            level_list.extend(rt)
        return level_list

    def subtree_levelorder_traversal(self):
        from queue import SimpleQueue
        q = SimpleQueue()
        q.put(self)

        keys = []
        while not q.empty():
            curr = q.get()
            keys.append(curr.key)

            if curr.left:
                q.put(curr.left)
            if curr.right:
                q.put(curr.right)

        return keys



  #  def subtree_inorder_traversal_iterative(self):
        

class BinarySearchTree:
    def __init__(self, Iterable=[], comp=operator.le):
        self.size = 0
        self.comp = comp
        self.root = None

        if Iterable:
            for k in Iterable:
                self.insert(k)

    def insert(self, key):
        tNode = TreeNode(key)
        if self.root:
            self.root.insert_iterative(tNode, self.comp)
        else:
            self.root = tNode
        self.size += 1

    def delete(self, key):
        tdel = self.root.subtree_search_recursive(key, self.comp)
        self.root.delete(tdel)
        self.size -= 1

    def inorder_traversal(self):
        if self.root:
            return self.root.subtree_inorder_traversal_recursive()
        else:
            return []

    def preorder_traversal(self):
        if self.root:
            return self.root.subtree_preorder_traversal_recursive()
        else:
            return []

    def postorder_traversal(self):
        if self.root:
            return self.root.subtree_postorder_traversal_recursive()
        else:
            return []

    def get_levels(self):
        if self.root:
            level = 1
            level_lists = []
            hasNode = True
            while hasNode:
                list = self.root.subtree_getlevel_recursive(level)
                if not list:
                    hasNode = False
                else:
                    level_lists.append(list)
                    level += 1
            return level_lists
        else:
            return []

    def levelorder_traversal(self):
        if self.root:
            return self.root.subtree_levelorder_traversal()
        else:
            return []

def bst_sort(arr, comp=operator.le):
    bst = BinarySearchTree(arr, comp)
    return bst.inorder_traversal()
