#!/usr/bin/env python3
# -*- coding: utf-8 -*-


class ListNode:
    def __init__(self, key, prior=None, posterior=None):
        self.key = key
        self.prior = prior
        self.posterior = posterior
        self._head = False

    def __str__(self):
        return self.key.__str__()

    def __del__(self):
        self.prior = None
        self.posterior = None
        del self.key

class LinkedListIterator:
    def __init__(self, node):
        self.node = node

    def __iter__(self):
        return self

    def __next__(self):
        if self.node._head == True:
            raise StopIteration
        key = self.node.key
        self.node = self.node.posterior
        return key

class LinkedList:
    """A double linked circular list"""
    def __init__(self, data=[]):
        self.head = ListNode(None)
        self.head._head = True
        self.head.prior = self.head
        self.head.posterior = self.head
        self.size = 0
        for key in data:
            self.append(key)

    def __len__(self):
        return self.size

    def __str__(self):
        arrow = " <-> "
        s = "head <-> "
        for key in self:
            s += (str(key) + arrow)
        s += "head"
        return s

    def __repr__(self):
        return self.__str__()

    def __iter__(self):
        it = LinkedListIterator(self.head.posterior)
        return it

    def __contains__(self, item):
        if self.search(item):
            return True
        else:
            return False

    def __getitem__(self, key):
        if type(key) != type(0):
            raise TypeError
        if key >= len(self) or key < -len(self):
            raise IndexError
        tmp = self.head.posterior
        for i in range(key):
            tmp = tmp.posterior
        return tmp.key

    def __setitem__(self, key, value):
        if type(key) != type(0):
            raise TypeError
        if key >= len(self) or key < -len(self):
            raise IndexError
        tmp = self.head.posterior
        for i in range(key):
            tmp = tmp.posterior
        tmp.key = value

    def __delitem__(self, key):
        if type(key) != type(0):
            raise TypeError
        if key >= len(self) or key < -len(self):
            raise IndexError
        tmp = self.head.posterior
        for i in range(key):
            tmp = tmp.posterior
        tmp.prior.posterior = tmp.posterior
        tmp.posterior.prior = tmp.prior
        del tmp
        self.size -= 1

    def search(self, key):
        tmp = self.head.posterior
        while tmp != self.head and tmp.key != key:
            tmp = tmp.posterior
        return tmp if tmp != self.head else None

    def append(self, key):
        newNode = ListNode(key)
        self.head.prior.posterior = newNode
        newNode.prior = self.head.prior
        newNode.posterior = self.head
        self.head.prior = newNode
        self.size += 1

    def prepend(self, key):
        newNode = ListNode(key)
        self.head.posterior.prior = newNode
        newNode.posterior = self.head.posterior
        self.head.posterior = newNode
        newNode.prior = self.head
        self.size += 1

    def delete(self, key):
        tmp = self.search(key)
        if tmp:
            tmp.prior.posterior = tmp.posterior
            tmp.posterior.prior = tmp.prior
            del tmp
            self.size -= 1

    

