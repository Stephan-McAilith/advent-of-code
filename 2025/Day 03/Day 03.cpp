#include <iostream>
#include <fstream>
#include <sstream>
#include <set>

unsigned long long findJoltage(std::istream& batteryBanks, int nbBatteryToUse) {
	std::string batteryBank;
	unsigned long long result = 0;

	while (std::getline(batteryBanks, batteryBank)) {
		unsigned long long batteriesJoltage = 0;

		for (unsigned int startIndex = 0, nbBatteriesLeftToSelect = nbBatteryToUse; nbBatteriesLeftToSelect; startIndex += 1, nbBatteriesLeftToSelect -= 1) {
			unsigned char bestBattery = 0;

			for (unsigned int i = startIndex; i <= batteryBank.size() - nbBatteriesLeftToSelect; i += 1) {
				if (batteryBank[i] > bestBattery) {
					bestBattery = batteryBank[i];
					startIndex = i;
				}
			}
			batteriesJoltage *= 10;
			batteriesJoltage += bestBattery - '0';
		}
		result += batteriesJoltage;
	}
    return result;
}

unsigned long long ex1(char* inputfile) {

    std::ifstream input(inputfile);
    return findJoltage(input, 2);
}

unsigned long long ex2(char* inputfile) {
    std::ifstream input(inputfile);
    return findJoltage(input, 12);
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