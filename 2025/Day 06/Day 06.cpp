#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>
#include <numeric>

unsigned long long ex1(char* inputfile) {
	std::ifstream istream(inputfile);
    std::string line;
    std::vector<std::stringstream> lines;
    unsigned long long grandTotal = 0;

    while (std::getline(istream, line))
        lines.push_back(std::stringstream(line));
    int size = lines.size();
    std::stringstream& operators = lines[size - 1];

    char op;
    while (bool(operators >> op)) {
        unsigned long long operand;
        unsigned long long total;
        std::vector<unsigned long long> operands;
        for (auto it = ++lines.rbegin(); it != lines.rend(); it++) {
            (*it) >> operand;
            operands.push_back(operand);
        }
        if (op == '+')
            total = std::accumulate(operands.begin(), operands.end(), 0LL);
        else
            total = std::accumulate(operands.begin(), operands.end(), 1LL, std::multiplies<unsigned long long>());
        grandTotal += total;
    }
    return grandTotal;
}

unsigned long long ex2(char* inputfile) {
    std::ifstream istream(inputfile);
    std::string line;
    std::vector<std::stringstream> lines;
    unsigned long long grandTotal = 0;

    while (std::getline(istream, line))
        lines.push_back(std::stringstream(line));
    int size = lines.size();
    std::stringstream operators = std::move(lines[size - 1]);
    lines.pop_back();

    char op;
    while (bool(operators >> op)) {
        int columnWitdh = 1;
        for (; operators.peek() == ' '; columnWitdh += 1, operators.get());
        if (operators.peek() != std::char_traits<char>::eof())
            columnWitdh -= 1;
        unsigned long long total;
        std::vector<unsigned long long> operands;
        for (int i = 0; i < columnWitdh; i += 1) {
            std::string operand;
            for (auto it = lines.begin(); it != lines.end(); it++) {
                operand += it->get();
            }
            operands.push_back(stoll(operand));
        }
        if (operators.peek() != std::char_traits<char>::eof())
            for (auto it = lines.begin(); it != lines.end(); it->get(), it++);
            
        if (op == '+')
            total = std::accumulate(operands.begin(), operands.end(), 0LL);
        else
            total = std::accumulate(operands.begin(), operands.end(), 1LL, std::multiplies<unsigned long long>());
        grandTotal += total;
    }
    return grandTotal;
}

int main(int ac, char* arg[])
{
    if (ac < 2) {
        std::cout << "Missing input file argument" << std::endl;
        return 1;
    }

    std::cout << "Ex 1: " << ex1(arg[1]) << std::endl;
    std::cout << "Ex 2: " << ex2(arg[1]) << std::endl;
}