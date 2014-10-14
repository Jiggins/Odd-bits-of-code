#include <iostream>
#include <stack>
using namespace std;

class cell {
	short val;
	public:
		void inc() {
			val = (val < 255) ? ++val : 0;
		}

		void dec() {
			val = (val > 0) ? --val : 255;
		}

		void show() {
			cout << val << endl;
		}

		char get() {
			return (char)val;
		}

		void takeInput() {
			short temp;
			cin >> temp;
			if (temp < 0 || temp > 255) {
				cout << "Invalid number";
			}
			else {
				val = temp;
			}
		}

		void set(short newval) {
			val = newval;
		}

		bool isZero() {
			return val;
		}
};

static int currentCell = 0;
static cell cells [30000];
static int instructionPointer = 0;
// Stack holds jump back points, similar to labels in Assembly.
static stack <int> jumpPoints; 

/* Brainfuck operations */

void shiftLeft() {
	currentCell = (currentCell > 0) ? --currentCell : 29999;
}

void shiftRight() {
	currentCell = (currentCell < 29999) ? ++currentCell : 0; 
}

void inc() {
	cells[currentCell].inc();
}

void dec() {
	cells[currentCell].dec();
}

char get() {
	return cells[currentCell].get();
}

void takeInput() {
	cells[currentCell].takeInput();
}

void showCells(int low, int high) {
	for (int i = min(low,high); i < max(low,high); i++) {
		cout << cells[i].get() << '\t';
	}
	cout << endl;
}

void showCurrent() {
	cout << "Slot:\t" << "Val:\t" << endl;
	cout << currentCell << '\t' << (short)cells[currentCell].get() << "\n\n";
}

bool isZero() {
	return !cells[currentCell].isZero();
}

/* Switch statement to switch between operations. */
void parseChar(char c, int index) {
	switch (c) {
		case '<': shiftLeft();            break;
		case '>': shiftRight();           break;
		case '+': inc();                  break;
		case '-': dec();                  break;
		case '.': cout << get();          break;
		case ',': takeInput();            break;
		case '[': jumpPoints.push(index); break;
		case ']': 
			if (isZero()) {
				jumpPoints.pop();
				break;
			}
			instructionPointer = jumpPoints.top();
			break;
		default: break;
	}
}

int main() {
	// Zero out cells.
	for (int i = 0; i < 30000; i++) {
		cells[i].set(0);
	}

	// Hello World!
	char expression [120] = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
	cout << expression << endl;

	// Instruction pointer loop.
	// Will jump back when ] is read and cell /= 0.
	for (instructionPointer = 0; expression[instructionPointer] != '\0'; instructionPointer ++) {
		parseChar(expression[instructionPointer], instructionPointer);
	}
}
