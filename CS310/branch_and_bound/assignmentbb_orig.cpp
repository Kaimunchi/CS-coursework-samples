/**
 * A program that implements branch-and-bound for the job assignment problem
 * @author originally written by Jon Beck
 * @author modified by Your Name Here
 * @version date last modified here
 */

#include <iostream>
#include <climits>
#include <queue>
#include <vector>
#include "matrix.h"

using namespace std;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"
class Node
{
 private:
  static unsigned next_label;
  unsigned label;
  unsigned level; // this is equivalent to the worker
  vector<unsigned> jobpath;
  vector<bool> jobsused;
  unsigned lower_bound;

 public:
  explicit Node(unsigned initial_level)
    : level{initial_level}
  {
    label = next_label++;
  }

  unsigned get_lower_bound() const
  {
    return lower_bound;
  }

  string get_label() const
  {
    return std::to_string(label);
  }

  unsigned get_level() const
  {
    return level;
  }

  vector<unsigned>& get_jobpath()
  {
    return jobpath;
  }

  void set_jobpath(vector<unsigned>& new_jobpath)
  {
    jobpath = new_jobpath;
  }

  vector<bool>& get_jobsused()
  {
    return jobsused;
  }

  void set_jobsused(vector<bool>& new_jobsused)
  {
    jobsused = new_jobsused;
  }

  string to_string() const
  {
    string result = "label:" + std::to_string(label) + ";level:" +
      std::to_string(level) + ";jobs:";
    for (auto job : jobpath)
      result += std::to_string(job) + ",";
    result.pop_back();
    result += ";lower_bound:" + std::to_string(lower_bound);
    return result;
  }

  void calculate_lower_bound(const Matrix<unsigned> & cost)
  {
    lower_bound = 0;
    for (size_t i = 0; i < jobpath.size(); i++)
    {
      lower_bound += cost.at(i, jobpath.at(i));
    }

    for (size_t row = jobpath.size(); row < cost.numrows(); row++)
    {
      unsigned smallest = UINT_MAX;
      for (size_t job = 1; job < cost.numrows(); job++)
      {
        if (cost.at(row, job) < smallest)
        {
          smallest = cost.at(row, job);
        }
      }
      lower_bound += smallest;
    }
  }
};
#pragma clang diagnostic pop

unsigned Node::next_label {0}; // define and initialize static class member

/**
 * a class simply to provide a less-than for priority queue Nodes
 * the less-than is reversed to greater-than because STL priority queue
 * is a max-heap and we need a min-heap
 */
class LBLessThan
{
 public:
  bool operator()(const Node & lhs, const Node & rhs)
  {
    if (lhs.get_lower_bound() == rhs.get_lower_bound())
    {
      return lhs.get_label() > rhs.get_label();
    }
    return lhs.get_lower_bound() > rhs.get_lower_bound();
  }
};

int main()
{
  // read n on std input
  unsigned n;
  cin >> n;

  Matrix<unsigned> cost(n + 1, n + 1);

  // read the cost matrix from stdin
  for (unsigned column = 0; column < n + 1; column++)
  {
    cost.at(0, column) = 0;
  }

  for (unsigned row = 1; row < n + 1; row++)
  {
    cost.at(row, 0) = 0;
    for (unsigned column = 1; column < n + 1; column++)
    {
      cin >> cost.at(row, column);
    }
  }

  priority_queue<Node, vector<Node>, LBLessThan> node_pq;

  Node node{0};
  node.get_jobpath().push_back(0);
  node.calculate_lower_bound(cost);
  for (unsigned i = 0; i <= n; i++)
  {
    node.get_jobsused().push_back(false);
  }
  node_pq.push(node);

  unsigned best_solution = UINT_MAX;

  size_t considering_counter {0};

  cout << "starting node: " << node.to_string() << endl;

  while(!node_pq.empty())
  {
    Node curnode = node_pq.top();
    node_pq.pop();
    considering_counter++;
    cout << "considering: " << curnode.get_label() << endl;
    if (curnode.get_level() == n)
    {
      if (curnode.get_lower_bound() < best_solution)
      {
        best_solution = curnode.get_lower_bound();
        cout << "new best solution: " << best_solution << endl;
      }
      else
      {
        cout << "discarding" << endl;
      }
    }
    else
    {
      for (unsigned i = 1; i <= n; i++)
      {
        if (!curnode.get_jobsused().at(i))
        {
          Node newnode {curnode.get_level() + 1};
          newnode.set_jobpath(curnode.get_jobpath());
          newnode.get_jobpath().push_back(i);
          newnode.calculate_lower_bound(cost);
          if (newnode.get_lower_bound() < best_solution)
          {
            newnode.set_jobsused(curnode.get_jobsused());
            newnode.get_jobsused().at(i) = true;
            node_pq.push(newnode);
            cout << "new node: " << newnode.to_string() << endl;
          }
        }
      }
    }
  }
  cerr << "Considered: " << considering_counter << endl;
  return 0;
}
