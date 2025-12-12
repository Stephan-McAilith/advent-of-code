#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>
#include <set>
#include <map>
#include <ranges>
#include <functional>
#include <iterator>
#include <numeric>

bool checkButtonCombinations(
    const std::string& startSequence,
    int combinationSize,
    std::vector<std::set<int>>::iterator rangeStart,
    const std::vector<std::set<int>>::iterator &rangeEnd,
    std::map<int, int> combination)
{
    if (combinationSize == 0) {
        for (int i = 0; i < startSequence.size(); i += 1) {
            int toggle = startSequence[i] == '#';
            if (combination[i] % 2 != toggle)
                return false;
        }
        return true;
    }

    for (; rangeStart != rangeEnd;) {
        std::map<int, int> newCombination = combination;
        for (auto it = rangeStart->begin(); it != rangeStart->end(); it++)
            newCombination[*it] += 1;
        if (checkButtonCombinations(startSequence, combinationSize - 1, ++rangeStart, rangeEnd, newCombination))
            return true;
    }
    return false;
}

unsigned long long ex1(char *inputfile)
{
    std::ifstream istream(inputfile);
    std::string tmp;
    unsigned long long buttonPushed = 0;
    while (std::getline(istream, tmp)) {
        std::istringstream line(tmp);
        std::string startSequence;
        std::vector<std::set<int>> buttons;
        while (std::getline(line, tmp, ' ')) {
            if (tmp[0] == '[')
                startSequence = tmp.substr(1, tmp.size() - 2);
            else if (tmp[0] == '(') {
                std::set<int> wiring;
                int wire;
                std::istringstream wiringStream(tmp.substr(1, tmp.size() - 2));
                while (wiringStream) {
                    (wiringStream >> wire).get();
                    wiring.insert(wire);
                }
                buttons.push_back(std::move(wiring));
            }
        }
        for (int i = 1; i <= buttons.size(); i += 1) {
            if (checkButtonCombinations(startSequence, i, buttons.begin(), buttons.end(), std::map<int, int>())) {
                buttonPushed += i;
                break;
            }
        }
    }
    return buttonPushed;
}


#define EPSILON 1e-9

class Machine {
public:
    Machine(std::vector<int> joltage, std::vector<std::vector<int>> buttons) : joltage(joltage), buttons(buttons) {
        height = joltage.size();
        width = buttons.size();
        matrix.resize(height, std::vector<double>(width + 1));

        for (int i = 0; i < height; i += 1)
            matrix[i][width] = joltage[i];
       
        for (int i = 0; i < width; i += 1) {
            for (int j : buttons[i]) {
                matrix[j][i] = 1;
            }
        }
    };
    void printMatrix() {
        std::cout << std::endl;

        for (auto r : matrix) {
            for (auto c : r) {
                std::cout << c << " ";
            }
            std::cout << std::endl;
        }
        std::cout << std::endl;

    }

    unsigned long long computeSmallestCombination() {
        gaussianElimination();
        max = std::ranges::max(joltage);
        dfs(std::vector<int>(variables.size()), 0);
        return min;
    }

private:
    void gaussianElimination() {
        int pivot = 0;
        for (int col = 0; col < width; col += 1) {
            double max = 0.;
            int maxRow = 0;
            for (int i = pivot; i < height; i += 1) {
                if (abs(matrix[i][col]) > max) {
                    max = abs(matrix[i][col]);
                    maxRow = i;
                }
            }
            if (max < EPSILON) {
                variables.push_back(col);
                continue;
            }
            dependent.push_back(col);

            if (maxRow != pivot) {
                std::swap(matrix[pivot], matrix[maxRow]);
            }

            double value = matrix[pivot][col];
            for (double& elem : matrix[pivot])
                elem /= value;

            for (int i = 0; i < height; i += 1) {
                if (i == pivot)
                    continue;
                double factor = matrix[i][col];
                if (abs(factor) < EPSILON)
                    continue;
                auto simplifiedRow = std::views::zip_transform([factor](double a, double b) {return a - b * factor; }, matrix[i], matrix[pivot]);
                matrix[i] = simplifiedRow | std::ranges::to<std::vector<double>>();
            }
            pivot += 1;
        }
    }
    void checkCombination(const std::vector<int> &pushes) {
        
        auto resultView = std::views::enumerate(dependent)
        | std::views::transform([&](auto enumeration) {
            auto [index, _] = enumeration;
            return std::ranges::fold_left(std::views::zip(variables, pushes), matrix[index][width], [&](double acc, auto tuple) {
                auto [col, push] = tuple;
                return acc - (matrix[index][col] * push);
            });
        });

        if (std::all_of(resultView.cbegin(), resultView.cend(), [](double result) {return result > -EPSILON && abs(result - round(result)) < EPSILON; })) {
            std::vector<double> results =  resultView | std::ranges::to<std::vector<double>>();
            int total = std::ranges::fold_left(resultView, 0, [](int sum, double result) {return sum + (int)result; }) + std::accumulate(pushes.begin(), pushes.end(), 0);
            min = std::min(min, total);
        }
    };

    void dfs(std::vector<int> pushes, int index) {
        if (index == pushes.size()) {
            checkCombination(pushes);
            return;
        }

        int localMax = joltage[variables[index]];
        for (int i = 0; i <= max; i += 1) {
            pushes[index] = i;
            if (std::accumulate(pushes.begin(), pushes.end(), 0) > min)
                break;
            dfs(pushes, index + 1);
        }
    }

private:
    std::vector<int> dependent;
    std::vector<int> variables;
    int min = INT32_MAX;
    int max = 0;
    unsigned int width, height;
    std::vector<std::vector<double>> matrix;
    std::vector<int> joltage;
    std::vector<std::vector<int>> buttons;
};



unsigned long long ex2(char* inputfile)
{
    std::ifstream istream(inputfile);
    std::string tmp;
    int counter = 0;
    unsigned long long buttonPushed = 0;
    while (std::getline(istream, tmp)) {
        std::istringstream line(tmp);
        std::vector<std::vector<int>> buttons;
        std::vector<int> joltage;
        while (std::getline(line, tmp, ' ')) {
            if (tmp[0] == '[')
                continue;
            std::vector<int> values;
            int value;
            std::istringstream vstream(tmp.substr(1, tmp.size() - 2));
            while (vstream) {
                (vstream >> value).get();
                values.push_back(value);
            }
            if (tmp[0] == '(')
                buttons.push_back(std::move(values));
            else
                joltage = std::move(values);
        }
        Machine machine(joltage, buttons);
        buttonPushed += machine.computeSmallestCombination();
        //char c;
        //std::cin >> c;
    }
    return buttonPushed;
}

int main(int ac, char *arg[])
{
    if (ac < 2)
    {
        std::cout << "Missing input file argument" << std::endl;
        return 1;
    }

    std::cout << "Ex 1: " << ex1(arg[1]) << std::endl;
    std::cout << "Ex 2: " << ex2(arg[1]) << std::endl;
}



//bool checkButtonCombinationsJoltage(
//    const std::vector<int>& joltage,
//    int combinationSize,
//    std::vector<std::vector<int>>::iterator rangeStart,
//    const std::vector<std::vector<int>>::iterator& rangeEnd,
//    std::map<int, int> combination)
//{
//    if (combinationSize == 0) {
//        for (int i = 0; i < joltage.size(); i += 1) {
//            int toggle = joltage[i] == '#';
//            if (combination[i] != joltage[i])
//                return false;
//        }
//        return true;
//    }
//
//    for (; rangeStart != rangeEnd; rangeStart++) {
//        std::vector<std::vector<int>>::iterator next = rangeStart;  next++;
//        std::map<int, int> newCombination = combination;
//        for (int i = 0; i <= combinationSize; i += 1) {
//            if (checkButtonCombinationsJoltage(joltage, combinationSize - i, next, rangeEnd, newCombination))
//                return true;
//            for (auto it = rangeStart->begin(); it != rangeStart->end(); it++)
//                newCombination[*it] += 1;
//        }
//    }
//    return false;
//}
//
//unsigned long long ex2(char* inputfile)
//{
//    std::ifstream istream(inputfile);
//    std::string tmp;
//    unsigned long long buttonPushed = 0;
//    while (std::getline(istream, tmp)) {
//        std::istringstream line(tmp);
//        std::string startSequence;
//        std::vector<std::vector<int>> buttons;
//        std::vector<int> joltage;
//        while (std::getline(line, tmp, ' ')) {
//            if (tmp[0] == '[')
//                continue;
//            std::vector<int> values;
//            int value;
//            std::istringstream vstream(tmp.substr(1, tmp.size() - 2));
//            while (vstream) {
//                (vstream >> value).get();
//                values.push_back(value);
//            }
//            if (tmp[0] == '(')
//                buttons.push_back(std::move(values));
//            else
//                joltage = std::move(values);
//        }
//        int i = 1;
//        for (; !checkButtonCombinationsJoltage(joltage, i, buttons.begin(), buttons.end(), std::map<int, int>()); i += 1);
//        buttonPushed += i;
//    }
//    return buttonPushed;
//}




//int checkButtonCombinationsJoltage(
//    std::vector<int> joltage,
//    std::vector<std::vector<int>>& buttons,
//    int buttonCount)
//{
//    int sum = 0;
//    for (auto it = joltage.begin(); it != joltage.end(); it++) {
//        if (*it < 0)
//            return 0;
//        sum += *it;
//    }
//
//    if (sum == 0)
//        return buttonCount;
//
//    std::vector<std::tuple<double, double, std::vector<std::vector<int>>::iterator>> aled;
//    for (auto it = buttons.begin(); it != buttons.end(); it++) {
//        std::vector<int> tmpJolt = joltage;
//        for (auto it2 = it->begin(); it2 != it->end(); it2++) {
//            tmpJolt[*it2] -= 1;
//        }
//        double mean = ((double)std::accumulate(tmpJolt.begin(), tmpJolt.end(), 0)) / (double)tmpJolt.size();
//        double diff = 0;
//        for (auto it2 = tmpJolt.begin(); it2 != tmpJolt.end(); it2++)
//            diff += abs(mean - double(*it2));
//        aled.push_back(std::make_tuple(mean, diff, it));
//    }
//    std::sort(aled.begin(), aled.end(), [](auto const& a, auto const& b) {return std::get<0>(a) < std::get<0>(b) || (std::get<0>(a) == std::get<0>(b) && std::get<1>(a) < std::get<1>(b)); });
//    for (auto it = aled.begin(); it != aled.end(); it++) {
//        std::vector<int> tmpJolt = joltage;
//        for (auto it2 = std::get<2>(*it)->begin(); it2 != std::get<2>(*it)->end(); it2++) {
//            tmpJolt[*it2] -= 1;
//        }
//        int result = checkButtonCombinationsJoltage(tmpJolt, buttons, buttonCount + 1);
//        if (result != 0)
//            return result;
//    }
//    return 0;
//}



//std::vector<int> pushButton(std::vector<int> state, const std::vector<int>& button) {
//    for (int x : button)
//        state[x] += 1;
//    return state;
//}
//
//
//int computeCombination(const std::vector<int>& joltage, std::vector<std::vector<int>>buttons) {
//    std::set<std::vector<int>> states = { std::vector<int>(joltage.size()) };
//    std::set<std::vector<int>> seenStates = {};
//
//    for (int i = 1; ; ++i) {
//        auto stateView = states
//            | std::views::transform([&buttons](const std::vector<int>& state) {
//            return buttons
//                | std::views::transform([state](const auto& button) {
//                return pushButton(state, button); // returns std::vector<int>
//                    });
//                })
//            | std::views::join
//            | std::views::filter([&joltage](const std::vector<int>& state) {
//            // keep states where all elements are <= joltage
//            return std::ranges::all_of(
//                std::views::zip(state, joltage),
//                [](auto tuple) {
//                    auto [s, j] = tuple;
//                    return s <= j;
//                }
//            );
//                });
//        states = stateView | std::ranges::to<std::set<std::vector<int>>>();
//        if (states.find(joltage) != states.end())
//            return i;
//        std::set<std::vector<int>> result;
//        std::cout << i << " ";
//
//        std::set_difference(states.begin(), states.end(), seenStates.begin(), seenStates.end(), std::inserter(result, result.begin()));
//        for (auto s : result)
//            seenStates.insert(s);
//        states = std::move(result);
//    }
//}
