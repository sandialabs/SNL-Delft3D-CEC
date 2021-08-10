import os

logDirectory = r"./BuildLogs"
pdfDirectory = r"."

pdfFiles = os.listdir(pdfDirectory)
expectedDocuments = ["DIMR - Functional Design.pdf",
					 "DIMR - Manual.pdf",
					 "DIMR - Technical Design.pdf",
					 "DIMR - Technical Documentation.pdf",
					 "DIMR - Test Plan.pdf",
					 "DIMR - Test Report.pdf"]

print "##teamcity[testStarted name='PDF Generated']"
for doc in expectedDocuments:
    if doc not in pdfFiles:
       print "##teamcity[testFailed name='PDF Generated' message='{} - Not Generated']".format(doc.strip())
print "##teamcity[testFinished name='PDF Generated']"


logFiles = os.listdir(logDirectory)
for log in logFiles:
    name = log.strip("_Log.txt")
    path = os.path.join(logDirectory,log)
    print "##teamcity[testSuiteStarted name='{}']".format(name)

    fi = open(path, 'r')
    logLines = fi.readlines()

    print "##teamcity[testStarted name='Citation Warnings']"
    for line in logLines:
        text = line.replace("'", " ")
        text = text.replace("`", " ")
        if "Package natbib Warning: Citation" in line:
            print "##teamcity[testFailed name='Citation Warnings' message='{}']".format(text.strip())

    print "##teamcity[testFinished name='Citation Warnings']"


    print "##teamcity[testSuiteFinished name='{}']".format(name)
    fi.close()
