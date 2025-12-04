#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

unsigned long long ex1(char* inputfile) {
	std::ifstream istream(inputfile);
	std::string row;
	std::vector<std::string> paperRollMap;

	while (std::getline(istream, row))
		paperRollMap.push_back(row);
 
	int nbAccessiblePaperRoll = 0;
	int height = paperRollMap.size(), width = paperRollMap[0].size();
 
	for (int i = 0; i < height; i += 1) {
 		for (int j = 0; j < width; j += 1) {
 			if (paperRollMap[i][j] != '@') continue;
 			int nbRollAround = 0;
 			for (int x = -1; x <= 1; x += 1) {
 				for (int y = -1; y <= 1; y += 1) {
 					if (x == 0 && y == 0) continue;
 					int a = i + x, b = j + y;
 					if (a < 0 || a >= height || b < 0 || b >= width) continue;
 					nbRollAround += paperRollMap[a][b] == '@';
 				}
 			}
 			nbAccessiblePaperRoll += nbRollAround < 4;
 		}
	}
	return nbAccessiblePaperRoll;
}

unsigned long long ex2(char* inputfile) {
	std::ifstream istream(inputfile);
	std::string row;
	std::vector<std::string> paperRollMap;

	while (std::getline(istream, row))
		paperRollMap.push_back(row);

	int totalNbAccessiblePaperRoll = 0;
	int nbAccessiblePaperRoll = 0;
	int height = paperRollMap.size(), width = paperRollMap[0].size();

	do {
		nbAccessiblePaperRoll = 0;
		for (int i = 0; i < height; i += 1) {
			for (int j = 0; j < width; j += 1) {
				if (paperRollMap[i][j] != '@') continue;
				int nbRollAround = 0;
				for (int x = -1; x <= 1; x += 1) {
					for (int y = -1; y <= 1; y += 1) {
						if (x == 0 && y == 0) continue;
						int a = i + x, b = j + y;
						if (a < 0 || a >= height || b < 0 || b >= width) continue;
						nbRollAround += paperRollMap[a][b] == '@';
					}
				}
				if (nbRollAround < 4) {
					nbAccessiblePaperRoll += 1;
					paperRollMap[i][j] = '.';
				}
			}
		}
		totalNbAccessiblePaperRoll += nbAccessiblePaperRoll;
	} while (nbAccessiblePaperRoll != 0);
	return totalNbAccessiblePaperRoll;
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