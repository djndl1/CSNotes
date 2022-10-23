#!/usr/bin/env python3

from collections.abc import Iterable, Sequence
import unittest
from itertools import dropwhile, islice, chain, groupby, takewhile
from functools import reduce
from more_itertools import ichunked

class PinqTransformation(unittest.TestCase):
    def setUp(self) -> None:
        self.fruit = ['apple', 'orange', 'banana', 'pear', 
         'raspberry', 'peach', 'plum', '']
        
    def test_reduce_as_aggregate_sum(self) -> None:
        total_length = reduce(lambda a, b: a + b, map(len, self.fruit), 0)
        
        print(total_length)
        
    def test_reduce_as_aggregate_concatenate(self) -> None:
        total_length = len(reduce(lambda a, b: a + b, self.fruit, ''))
        
        print(total_length)

    def test_map_as_select(self):
        
        first_letter = map(lambda f: len(f), self.fruit)
        
        print(list(first_letter))
        
    def test_list_comprehension_as_select(self):
        lengths = [len(f) for f in self.fruit]
        
        print(list(lengths))
        
    def test_filter_as_where(self):
        long_enough = filter(lambda f: len(f) > 3, self.fruit)
        
        print(list(long_enough))
        
    def test_list_comprehension_as_where(self):
        long_enough = [f for f in self.fruit if len(f) > 3]
        
        print(list(long_enough))
        
    def test_list_comprehension_as_select_where(self):
        long_enough_lengths = [len(f) for f in self.fruit if len(f) > 5]
        
        print(long_enough_lengths)
        
    def test_sorted_as_order_by(self) -> None:
        sorted_by_length = sorted(self.fruit, key=lambda f: len(f))
        sorted_by_length_reversed = sorted(self.fruit, key=lambda f: len(f), reverse=True)
        
        print(sorted_by_length)
        print(sorted_by_length_reversed)
        
    def test_reversed_as_reverse(self) -> None:
        reversed_fruits = reversed(self.fruit)
        
        self.assertSequenceEqual(self.fruit, list(reversed_fruits)[::-1])
        
    def test_slice_as_fake_reverse(self) -> None:
        reversed_fruits = self.fruit[::-1]
        
        self.assertSequenceEqual(self.fruit, list(reversed_fruits)[::-1])
        
    def test_customized_helper_as_distinct(self) -> None:
        def distinct(it: Iterable) -> Iterable:
            seen = set()
            for s in it:
                if s not in seen:
                    seen.add(s)
                    yield s
                    
        duplicated = self.fruit + self.fruit
        
        unique = distinct(duplicated)
        
        self.assertEqual(len(duplicated), sum(1 for u in unique) * 2)
        
class PinqExpansion(unittest.TestCase):
    def setUp(self) -> None:
        self.fruit = ['apple', 'orange', 'banana', 'pear', 
         'raspberry', 'peach', 'plum']
        
    def test_chain_as_concat(self) -> None:
        fruit2 = ['Big Apple', 'Arancia']
        
        concatenated = chain(self.fruit, fruit2)
        
        print(list(concatenated))
    
    
class PinqReduction(unittest.TestCase):
    def setUp(self) -> None:
        self.fruit = ['apple', 'orange', 'banana', 'pear', 
         'raspberry', 'peach', 'plum']
        
    def test_any_with_list_comprehension(self) -> None:
        self.assertTrue(any("r" in f for f in self.fruit))
        
    def test_all_with_list_comprehension(self) -> None:    
        self.assertFalse(all("r" in f for f in self.fruit))
        
    def test_as_contains(self) -> None:
        '''
            https://docs.python.org/3.10/reference/expressions.html#membership-test-details
        '''
        self.assertTrue('apple' in self.fruit) # works for plain lists
        self.assertTrue('apple' in iter(self.fruit)) # also works for iterators
        self.assertTrue('apple' in chain(self.fruit)) # also works for iterators
        
    def test_min_max(self) -> None:
        min_fruit = min(self.fruit)
        max_fruit = max(self.fruit)
        
        print(min_fruit, max_fruit);
        
    def test_min_max_by_key(self) -> None:
        min_fruit = min(self.fruit, key=lambda f: len(f))
        max_fruit = max(self.fruit, key=lambda f: len(f))
        
        print(min_fruit, max_fruit);
        
    def test_len_as_count(self):
        print(len(self.fruit))
        
    def test_sum_as_count(self):
        it = iter(self.fruit)
        
        cnt = sum([1 for f in self.fruit])
        
        print(cnt)
        
    def test_next_as_first(self) -> None:
        first_fruit = next(iter(self.fruit))
        
        self.assertEqual("apple", first_fruit)
        
    def test_next_as_first_or_default(self) -> None:
        first_or_default_fruit = next(iter(self.fruit))
        
        self.assertEqual("apple", first_or_default_fruit)
        
        first_or_default_none = next(iter([]), None)
        self.assertIsNone(first_or_default_none)
        
    def test_zip_as_zip(self) -> None:
        numbers: Sequence[int] = [
            1, 2, 3, 4, 5, 6, 7
        ]
        letters: Sequence[str] = [
            'A', 'B', 'C', 'D', 'E', 'F'
        ]
        
        zipped = zip(numbers, letters)
        
        print(dict(zipped))

class PinqSetOperations(unittest.TestCase):
    def setUp(self) -> None:
        self.fruit = ['apple', 'orange', 'banana', 'pear', 'apple'
         'raspberry', 'peach', 'plum', '']
        
    def test_difference_as_except(self) -> None:
        fruit2 = ['raspberry', 'peach', 'plum', '']
        
        excepted = set(self.fruit).difference(fruit2)
       
        print(excepted)
        
    def test_comprehension_as_except(self) -> None:
        fruit2 = ['raspberry', 'peach', 'plum', '']
        
        excepted ={m for m in self.fruit if m not in fruit2}
        
        print(excepted)
        
    def test_set_union_as_union(self) -> None:
        fruit2 = ['raspberry', 'peach', 'plum', '2']
        
        unioned = set(self.fruit).union(fruit2)
        
        print(unioned)
        
    def test_comprehension_as_intersection(self) -> None:
        fruit2 = ['raspberry', 'peach', 'plum', '2']
        
        
        
        
class PinqMoreIterTools(unittest.TestCase):
    '''
        also available at https://pypi.org/project/more-itertools/
    '''
    def setUp(self) -> None:
        self.fruit = ['apple', 'orange', 'banana', 'pear', 
         'raspberry', 'peach', 'plum', '']
        
    def test_append(self) -> None:
        appended = chain(self.fruit, ["UnknownFruit"])
        
        self.assertEqual("UnknownFruit", list(appended)[-1])
        
    def test_prepend(self) -> None:
        prepended = chain(["UnknownFruit"], self.fruit)
        self.assertEqual("UnknownFruit", next(prepended))
        
    def test_sorted_groupby_as_groupby(self) -> None:
        grouped = groupby(sorted(self.fruit, key=len), key=len)
        
        for k, g in grouped:
            print(f"{k}: {list(g)}")
            
    def test_islice_as_take(self) -> None:
        taken2 = islice(self.fruit, 2)
        
        print(list(taken2))
        
    def test_islice_as_skip(self) -> None:
        skipped2 = islice(self.fruit, 2, None)
        
        print(list(skipped2))
        
    def test_islice_as_skip_take(self) -> None:
        PAGE_SIZE = 2
        paged2 = islice(self.fruit, PAGE_SIZE * 1, PAGE_SIZE * 2)
        
        print(list(paged2))
        
    def test_takewhile_as_take_while(self) -> None:
        taken = takewhile(lambda f: len(f) <= 7, self.fruit)
        
        print(list(taken))
        
    def test_dropwhile_as_skip_while(self) -> None:
        taken = dropwhile(lambda f: len(f) <= 7, self.fruit)
        
        print(list(taken))
        
    def test_ichunked_as_chunk(self) -> None:
        batched = ichunked(self.fruit, 2)
        
        for batch in batched:
            print(list(batch))
        