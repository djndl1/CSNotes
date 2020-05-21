#include <vector>
#include <algorithm>
#include <stack>
#include <queue>
#include <type_traits>
#include <utility>

template <typename DataType>
struct TreeNode {
    using TreeNodePtr = TreeNode*;

    DataType key;
    TreeNodePtr parent;
    TreeNodePtr left;
    TreeNodePtr right;

    TreeNode(const DataType& data)
        : key{data}, parent{nullptr}, left{nullptr}, right{nullptr}
    { }

    TreeNode( DataType&& data)
        : key{std::move(data)}, parent{nullptr}, left{nullptr}, right{nullptr} {}
    TreeNode(const TreeNode&) = delete;
    TreeNode& operator=(const TreeNode&) = delete;

    TreeNode(TreeNode&& other)
        : DataType{std::move(other.key)}, parent{other.parent}, left{other.left}, right{other.right}
    {
        other.parent = other.left = other.right = nullptr;
    }

    TreeNode& operator=(TreeNode&& other)
    {
        std::swap(key, other.key);
        parent = other.parent;
        left = other.left;
        right = other.right;
        other.parent = other.left = other.right = nullptr;
    }

    TreeNodePtr subtree_leftmost_iterative()
    {
        TreeNodePtr n = this;
        while (n->left != nullptr)
            n = n->left;
        return n;
    }

    TreeNodePtr subtree_leftmost_recursive()
    {
        if (this->left)
            return this->left->subtree_leftmost_recursive();
        else
            return this;
    }

    TreeNodePtr subtree_rightmost_iterative()
    {
        TreeNodePtr n = this;
        while (n->right != nullptr)
            n = n->right;
        return n;
    }

    TreeNodePtr subtree_rightmost_recursive()
    {
        if (this->right)
            return this->right->subtree_rightmost_recursive();
        else
            return this;
    }

    template <typename Comparator>
    TreeNodePtr subtree_search_recursive(const DataType& key, const Comparator& comp)
    {
        if (this->key == key)
            return this;
        if (comp(key, this->key))
            return this->left->subtree_search_recursive(key, comp);
        else
            return this->right->subtree_search_recursive(key, comp);
    }

    template <typename Comparator>
    TreeNodePtr subtree_search_iterative(const DataType& key, const Comparator& comp)
    {
        TreeNodePtr tmp = this;
        while (tmp and tmp->key != key) {
            if (comp(key, tmp->key))
                tmp = tmp->left;
            else
                tmp = tmp->right;
        }
        return tmp;
    }

    TreeNodePtr successor()
    {
        if (this->right)
            return this->right->subtree_leftmost_iterative();
        TreeNodePtr x = this;
        TreeNodePtr y = x->parent;
        while (y and y->right == x) {
            x = y;
            y = y->parent;
        }
        return y;
    }

    TreeNodePtr predecessor()
    {
        if (this->left)
            return this->left->subtree_rightmost_iterative();

        TreeNodePtr x = this;
        TreeNodePtr y = x->parent;
        while (y and y->left == x) {

          x = y;
          y = y->parent;
        }
        return y;
    }

    template <typename Comparator>
    void insert_recursive(TreeNodePtr node, Comparator comp)
    {
        if (not node)
            return;

        if (comp(node->key, this->key)) {
            if (left)
                left->insert_recursive(node, comp);
            else
                left = node;
        } else {
            if (right)
                right->insert_recursive(node, comp);
            else
                right = node;
        }
    }

    template <typename Comparator>
    void insert_iterative(TreeNodePtr node, const Comparator& comp)
    {
        TreeNodePtr x = this;
        TreeNodePtr y;

        while (x) {
            y = x;
            if (comp(node->key, x->key)) {
                x = x->left;
            } else
                x = x->right;
        }
        y = node->parent;
        if (comp(node->key, y->key))
            y->left = node;
        else
            y->right = node;
    }

    static void subtree_transplant(TreeNodePtr old, TreeNodePtr nov)
    {
        if (old->parent->left == old)
            old->parent->left = nov;
        else
            old->parent->right = nov;
        if (nov)
            nov->parent = old->parent;
    }

    void _delete(TreeNodePtr node)
    {
        if (not node)
            return;
        if (not node->left)
            subtree_transplant(node, node->right);
        else if (not node->right)
            subtree_transplant(node, node->left);
        else {
            TreeNodePtr y = node->right->subtree_leftmost_iterative();
            if (y->parent != node) {
                subtree_transplant(y, y->right);
                y->right = node->right;
                node->right->parent = y;
            }
            subtree_transplant(node, y);
            y->left = node->left;
            y->left->parent = y;
        }
    }

    std::vector<DataType> subtree_preorder_traversal_recursive()
    {
        std::vector<DataType> keys;
        keys.push_back(key);
        if (left) {
            auto lt = left->subtree_preorder_traversal_recursive();
            keys.reserve(lt.size());
            keys.insert(keys.end(),
                        std::make_move_iterator(lt.begin()),
                        std::make_move_iterator(lt.end()));
        }
        if (right) {
            auto rt = right->subtree_preorder_traversal_recursive();
            keys.reserve(rt.size() + keys.size());
            keys.insert(keys.end(),
                        std::make_move_iterator(rt.begin()),
                        std::make_move_iterator(rt.end()));
        }
        return keys;
    }

    std::vector<DataType> subtree_preorder_traversal_iterative()
    {
        std::vector<DataType> keys;
        std::stack<TreeNodePtr> it_stack;
        it_stack.push(this);

        while (not it_stack.empty()) {
            TreeNodePtr curr = it_stack.top();
            it_stack.pop();
            keys.push_back(curr);

            if (curr->right)
                it_stack.push(curr->right);
            if (curr->left)
                it_stack.push(curr->left);
        }

        return keys;
    }

    std::vector<DataType> subtree_postorder_traversal_recursive()
    {
        std::vector<DataType> keys;

        if (left) {
            auto lt = left->subtree_postorder_traversal_recursive();
            keys.reserve(lt.size());
            move(lt.begin(), lt.end(), std::back_inserter(keys));
        }
        if (right) {
          auto rt = left->subtree_postorder_traversal_recursive();
          keys.reserve(keys.size() + rt.size());
          move(rt.begin(), rt.end(), std::back_inserter(keys));
        }
        keys.push_back(key);

        return keys;
    }

    std::vector<DataType> subtree_inorder_traversal_recursive()
    {
        std::vector<DataType> keys;

        if (left) {
            auto lt = left->subtree_inorder_traversal_recursive();
            keys.reserve(lt.size());
            std::move(lt.begin(), lt.end(),
                      std::back_inserter(keys));
        }
        keys.push_back(key);
        if (right) {
          auto lt = right->subtree_inorder_traversal_recursive();
          keys.reserve(keys.size() + lt.size());
          std::move(lt.begin(), lt.end(), std::back_inserter(keys));
        }

        return keys;
    }

    std::vector<DataType> subtree_inorder_traversal_iterative()
    {
        std::vector<DataType> keys;
        std::stack<TreeNodePtr> it_stack;
        TreeNodePtr curr = this;

        while (curr or not it_stack.empty()) {
            while (curr) {
                it_stack.push(curr);
                curr = curr->left;
            }
            curr = it_stack.top();
            it_stack.pop();
            keys.push_back(curr->key);
            curr = curr->right;
        }

        return keys;
    }

    std::vector<DataType> subtree_levelorder_traversal()
    {
        std::queue<TreeNodePtr> q;
        std::vector<DataType> keys;

        q.push(this);

        while (q.empty()) {
            TreeNodePtr curr = q.front();
            q.pop();
            keys.push_back(curr->key);

            if (curr->left)
                q.push(curr->left);
            if (curr->right)
                q.push(curr->right);
        }

        return keys;
    }

    std::vector<DataType> subtree_getlevel_recursive(size_t level)
    {
        std::vector<DataType> levelData;
        if (level == 1) {
            return {key};
        }

        if (left) {
            auto lt = left->subtree_getlevel_recursive(level-1);
            levelData.reserve(lt.size());
            move(lt.begin(), lt.end(), std::back_inserter(levelData));
        }
        if (right) {
            auto lt = right->subtree_getlevel_recursive(level - 1);
            levelData.reserve(levelData.size() + lt.size());
            move(lt.begin(), lt.end(), std::back_inserter(levelData));
        }
        return levelData;
    }
};

template <typename DT, typename CT = std::less<DT>>
class BinarySearchTree {
private:
    size_t m_size;
    CT m_comp;
    typename TreeNode<DT>::TreeNodePtr m_root;

public:
    BinarySearchTree() : m_size{0}, m_comp{std::less<DT>{}}, m_root{nullptr}
    { }

    template<typename Iter>
    BinarySearchTree(Iter first, Iter end, CT comp)
        : m_comp{comp}
    {
        static_assert(std::is_same<
                      std::decay<typename Iter::value_type>,
                      DT>::value, "Wrong Iterator type");

        for (; first != end; first++) {
            insert(*first);
      }
    }

    void insert(const DT& key)
    {
        typename TreeNode<DT>::TreeNodePtr tNode = new TreeNode<DT>(key);
        if (m_root)
            m_root->insert_iterative(tNode);
        else
            m_root = tNode;
        m_size++;
    }

    void insert(DT&& key)
    {
        typename TreeNode<DT>::TreeNodePtr tNode = new TreeNode<DT>(std::move(key));
        if (m_root)
            m_root->insert_iterative(tNode);
        else
            m_root = tNode;
        m_size++;
    }

    void remove(const DT& key)
    {
        if (not m_root)
            return;
        typename TreeNode<DT>::TreeNodePtr tdel = m_root->subtree_search_recursive(key, m_comp);
        m_root->_delete(tdel);
        m_size--;
    }

    std::vector<DT> inorder_traversal()
    {
        if (m_root) {
            return m_root->subtree_inorder_traversal_recursive();
        }
        else
            return {};
    }
};

template<typename Iter, typename CT>
void bst_sort(Iter first, Iter last, CT comp)
{
    BinarySearchTree<typename Iter::value_type, CT>
        sort_tree(first, last, comp);

    auto sorted_vec = sort_tree.inorder_traversal();
    move(sorted_vec.begin(), sorted_vec.end(),
         first);
}
