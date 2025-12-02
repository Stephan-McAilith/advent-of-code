#include <iostream>
#include <fstream>
#include <sstream>
#include <set>

unsigned long long ex1(char* inputfile) {
    std::ifstream input(inputfile);
    std::string start, end;
    unsigned long long result = 0;

    while (std::getline(input, start, '-') && std::getline(input, end, ',')) {
        long long startVal = stoll(start);
        long long endVal = stoll(end);
        int halfLength = start.size() / 2;
        long long half = halfLength == 0 ? 0 : stoll(start.substr(0, halfLength));
      
        for (long long testVal = 0; testVal < endVal; half += 1) {
            testVal = half * std::pow(10, int(std::log10(half)) + 1) + half;
            if (testVal >= startVal && testVal <= endVal)
                result += testVal;
        } 
    }
    return result;
}

unsigned long long ex2(char* inputfile) {
    std::ifstream input(inputfile);
    std::string start, end;
    unsigned long long result = 0;
    std::set<long long> found;

    while (std::getline(input, start, '-') && std::getline(input, end, ',')) {
        long long startVal = stoll(start);
        long long endVal = stoll(end);
        int halfLength = (start.size() + 1) / 2;

        for (long long i = 1; true; i += 1) {
            long long testVal = i * std::pow(10, int(std::log10(i)) + 1) + i;
            if (testVal > endVal) break;
            while (testVal <= endVal) {
                if (testVal >= startVal && found.find(testVal) == found.end()) {
                    result += testVal;
                    found.insert(testVal);
                }
                testVal = testVal * std::pow(10, int(std::log10(i)) + 1) + i;
            }
        }
    }
    return result;
}


int main(int ac, char *arg[])
{
    if (ac < 2) {
        std::cout << "Missing input file argument" << std::endl;
        return 1;
    }

    std::cout << "Ex 1: " << ex1(arg[1]) << std::endl;
    std::cout << "Ex 2: " << ex2(arg[1]) << std::endl;
}