#include <iostream>
#include <fstream>
#include <sstream>



int ex1(char* inputfile) {  
    std::ifstream input(inputfile);
    std::istringstream lineStream;
    std::string line;
    int arrowPosition = 50;
    unsigned int zeroCount = 0;
    char rotationDirection;
    int rotationValue;
    while (std::getline(input, line)) {
        std::istringstream lineStream(line);
        lineStream >> rotationDirection;
        lineStream >> rotationValue;
        arrowPosition += rotationDirection == 'R' ? rotationValue : -rotationValue;
        arrowPosition %= 100;
        arrowPosition += arrowPosition < 0 ? 100 : 0;
        zeroCount += (arrowPosition == 0);
    }
    return zeroCount;
}

int ex2(char* inputfile) {
    std::ifstream input(inputfile);
    std::istringstream lineStream;
    std::string line;
    int arrowPosition = 50;
    int futurPosition = 50;
    unsigned int zeroCount = 0;
    char rotationDirection;
    int rotationValue;
    while (std::getline(input, line)) {
        std::istringstream lineStream(line);
        lineStream >> rotationDirection;
        lineStream >> rotationValue;
        zeroCount += rotationValue / 100;
        rotationValue %= 100;
        futurPosition = arrowPosition + (rotationDirection == 'R' ? rotationValue : -rotationValue);
        zeroCount += (futurPosition < 0 && arrowPosition > 0)  || futurPosition >= 100 || (futurPosition == 0 && rotationValue != 0);
        arrowPosition = futurPosition;
        arrowPosition += arrowPosition < 0 ? 100 : 0;
        arrowPosition %= 100;
    }
    return zeroCount;
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