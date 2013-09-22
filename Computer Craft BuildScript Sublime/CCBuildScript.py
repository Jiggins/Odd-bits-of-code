import os
import re
import shutil
import sys

# Shamefully stolen from Minecraft forge installer
def getMinecraftPath():
    if   sys.platform.startswith('linux'):
        return os.path.expanduser("~/.minecraft")
    elif sys.platform.startswith('win'):
        return os.path.join(os.getenv("APPDATA"), ".minecraft")
    elif sys.platform.startswith('darwin'):
        return os.path.expanduser("~/Library/Application Support/minecraft")
    else:
        print "Cannot detect of version : %s. Please report to your closest sysadmin"%sys.platform
        sys.exit()
# End of stolen code

def getWorldPath(world):
	return getMinecraftPath() + "/saves/" + world

def getComputerPath(world, id):
	return getWorldPath(world) + "/computer/" + id

def copyToWorld(world, id):
	thisFile = sys.argv[1]
	for i in id:
		computer = os.path.realpath(getComputerPath(world, str(i)))
		print "copying " + os.path.basename(thisFile) + " to computer ID " + str(i) + " in " + os.path.basename(world) 
		try:
			shutil.copy(thisFile, computer)
		except IOError:
			print "No such file or directory: " + computer

def readFile():
	filePath = sys.argv[1]

	file = open(filePath, 'r')
	world = file.next().strip()
	computer = file.next().strip()

	try:
		matchWorld = re.search("(--world=)(.*)", world)
		matchComputer = re.search("(--id=)(.*)", computer)
		matchedWorld = matchWorld.group(2)
		computerID = matchComputer.group(2)

		ranges = (x.split("-") for x in computerID.split(","))
		computers = [i for r in ranges for i in range(int(r[0]), int(r[-1]) + 1)]

		return matchedWorld, computers

	except:
		print "Plese specify the world with \"--world=[World Name]\"."
		print "Please specify the computer ID withuse \"--id=\"."
		sys.exit()

def main():
	world, computers = readFile()
	copyToWorld(world, computers)

if __name__ == '__main__':
	main()