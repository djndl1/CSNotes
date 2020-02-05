#include <gtest/gtest.h>

#include <functional>
#include <random>
#include <vector>

#include "sort.h"
#include "merge_sort.hh"

using std::is_sorted;
using std::vector;

std::vector<int> GenerateRandomVector(int NumberCount, int minimum, int maximum)
{
    std::random_device rd;
    std::mt19937 gen(rd());
    // these can be global and/or static, depending on how you use random elsewhere

    std::vector<int> values(NumberCount);
    std::uniform_int_distribution<> dis(minimum, maximum);
    std::generate(values.begin(), values.end(), [&]() { return dis(gen); });
    return values;
}

class TestSort : public ::testing::Test {
protected:
    void SetUp() override
    {
        limit = 1000;
        vec_num = 10000;
        lens = GenerateRandomVector(vec_num, 0, limit);
    }

    int limit;
    size_t vec_num;
    std::vector<int> lens;
};

TEST_F(TestSort, InsertionHandleEmptyInput)
{
    vector<int> vain {};
    insertion_sort(vain);
    ASSERT_TRUE(is_sorted(vain.begin(), vain.end()))
        << "insertion_sort() failed on empty input";
    insertion_sort_nonincreasing(vain);
    ASSERT_TRUE(is_sorted(vain.begin(), vain.end(), std::greater<int>()))
        << "insertion_sort_nonincreasing() failed on empty input";
    merge_sort(vain);
    ASSERT_TRUE(is_sorted(vain.begin(), vain.end()))
        << "merge_sort() failed on empty input";
    merge_sort(vain.begin(), vain.end());
    ASSERT_TRUE(is_sorted(vain.begin(), vain.end()))
        << "merge_sort() template failed on empty input";
}

TEST_F(TestSort, InsertionHandleSingleInput)
{
    for (int i = 0; i < vec_num; i++) {
        auto vec = GenerateRandomVector(1, 0, limit);
        insertion_sort(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
    }
}

TEST_F(TestSort, InsertionHandleRegularInput)
{
    for (auto len : lens) {
        auto vec = GenerateRandomVector(len, 0, limit);
        insertion_sort(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
    }
}

TEST_F(TestSort, InsertionHandleSingleInputForNonIncreasing)
{
    for (int i = 0; i < vec_num; i++) {
        auto vec = GenerateRandomVector(1, 0, limit);
        insertion_sort_nonincreasing(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin(), std::greater<int>()));
    }
}
TEST_F(TestSort, InsertionHandleRegularInputForNonIncreasing)
{
    for (auto len : lens) {
        auto vec = GenerateRandomVector(len, 0, limit);
        insertion_sort_nonincreasing(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin(), std::greater<int>()));
    }
}

TEST_F(TestSort, MergeSortSingleInput)
{
    for (int i = 0; i < vec_num; i++) {
        auto vec = GenerateRandomVector(1, 0, limit);
        merge_sort(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
    }
}

TEST_F(TestSort, MergeHandleRegularInput)
{
    for (auto len : lens) {
        auto vec = GenerateRandomVector(len, 0, limit);
        merge_sort(vec);
        ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
    }
}

TEST_F(TestSort, MergeSortTemplateSingleInput) {
  for (int i = 0; i < vec_num; i++) {
    auto vec = GenerateRandomVector(1, 0, limit);
    merge_sort(vec.begin(), vec.end());
    ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
  }
}

TEST_F(TestSort, MergeHandleTemplateRegularInput) {
  for (auto len : lens) {
    auto vec = GenerateRandomVector(len, 0, limit);
    merge_sort(vec.begin(), vec.end());
    ASSERT_TRUE(is_sorted(vec.begin(), vec.begin()));
  }
}

int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
