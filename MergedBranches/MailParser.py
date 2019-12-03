import mailparser
import glob
import email

Files = glob.glob('email/*.eml')
# print(Files)
ToFrom = []

outputPairs = open("BipartiteEdgeList.csv","w")

def grabAddress(inputTuple):
    (_, recipientAddress) = inputTuple
    return recipientAddress

for fileName in Files:
    print(fileName)
    mail = mailparser.parse_from_file(fileName)
    body = mail.body
    fromList = mail.from_
    if len(fromList) == 0 or len(fromList) > 1: 
        pass
    else:
        (_, senderAddress) = fromList[0]
        recipients = map(grabAddress, mail.to)
        time = mail.date
        message = mail.message
        senderPair = senderAddress + " , " + fileName + "\n"

        ToFrom.append(senderPair)
        for recipient in recipients:
            recipientPair = fileName + " , " + recipient + "\n"
            ToFrom.append(recipientPair)

# print(ToFrom)
# for item in ToFrom:
#     outputPairs.write(item + "\n")

outputPairs.writelines(ToFrom)
