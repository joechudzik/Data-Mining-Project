import mailparser
import glob
import email
import string
from collections import Counter
import time

def grabAddress(inputTuple):
    (_, recipientAddress) = inputTuple
    return recipientAddress

def writeAsCSVLine(inputTuple):
    (fromElem, toElem) = inputTuple
    return fromElem + "," + toElem + "\n"

def writeAsCSVLineWeighted(inputTriple):
    ((fromElem, toElem), count) = inputTriple
    return fromElem + "," + toElem + "," + str(count) + "\n"

def removeDuplicates(pair):
    (fromElem, toElem) = pair
    return fromElem != toElem

#useful for debugging without running through the entire email set.
#targetFiles = targetFiles[0:20]

start = time.time()

targetFiles = glob.glob('email/*.eml')
print(str(len(targetFiles)) + " files found.") 

#Initialize lists to store parsed values
bipartiteEdgeList = []                                                                              #type should be (str * str)
unorderedBipartiteEdgeList = []                                                                     #type should be (str * str)
emailNetworkEdgeList = []                                                                           #type should be (str * str)      
unreadableEmailsList = []                                                                           #Useful metadata for us.
emailAttributesList = []                                                                            #Not used yet, might be useful for timestamp information

bipartiteEdgeListCsv = open("BipartiteEdgeList.csv","w")
unorderedBipartiteEdgeListCsv = open("UnorderedBipartiteEdgeList.csv", "w")
emailAttributesCsv = open("EmailAttributes.csv", "w")
emailNetworkEdgeCsv = open("EmailNetworkEdgeList.csv", "w")
unreadableEmailsCsv = open("UnreadableEmails.csv", "w")

for thisFile in targetFiles:
    
    trimmedFileName = thisFile[6:]                                                                  # Removing "email/" from the location, so the filename can be printed nicely.
    mail = mailparser.parse_from_file(thisFile)
    
    fromList = mail.from_
    if  len(fromList) == 0:
        print(trimmedFileName + " had 0 \"From\" addresses - this email is likely unreadable" )
        unreadableEmailsList.append(trimmedFileName + "\n")

    elif len(fromList) > 1:
        print(trimmedFileName + " had >1 \"From\" addresses - this email is likely unreadable" )
        unreadableEmailsList.append(trimmedFileName + "\n")
    
    else:
        (_, senderAddress) = fromList[0]
        senderAddress = senderAddress.lower()
        recipientAddresses = map(grabAddress, mail.to)
        emailTime = mail.date
        #message = mail.message

        #Adding sender->email edge for bipartite graph
        senderToEmailEdge = (senderAddress.lower(), trimmedFileName.lower())
        bipartiteEdgeList.append(senderToEmailEdge)
        unorderedBipartiteEdgeList.append(senderToEmailEdge)
        #Write all attributes per email
        # emailAttributesList.append(trimmedFileName + "," + emailTime.__str__)

        for recipient in recipientAddresses:

            #Adding all email->recipient edges for bipartite graph
            emailToRecipientEdge = (trimmedFileName.lower(), recipient.lower())
            bipartiteEdgeList.append(emailToRecipientEdge)

            reversedEmailToRecipientEdge = (recipient.lower(), trimmedFileName.lower())
            unorderedBipartiteEdgeList.append(reversedEmailToRecipientEdge)

            #Adding all sender->recipient edges for nonBipartite graph
            senderToRecipientEdge = (senderAddress.lower(), recipient.lower())
            emailNetworkEdgeList.append(senderToRecipientEdge)
        
        #Good to get feedback on progress.
        print("Parsed " + trimmedFileName + " successfully.")

#Remove self-referential emails
#Maybe ToDo - record which duplicate references were removed?
preSelfReferenceRemoval = len(emailNetworkEdgeList)
emailNetworkEdgeList = list(filter(removeDuplicates, emailNetworkEdgeList))
postSelfReferenceRemoval = len(list(emailNetworkEdgeList))
print("The number of removed self-referential emails is: " 
    + str(preSelfReferenceRemoval - postSelfReferenceRemoval))

#Assign weights to elements
weightedEmailNetworkEdgeList = list(Counter(emailNetworkEdgeList).items())                          #type is ((str * str) * int)[]

#Write aggregate types into comma-separated strings to be written to file
bipartiteEdgeTextList = list(map(writeAsCSVLine, bipartiteEdgeList))
unorderedBipartiteEdgeTextList = list(map(writeAsCSVLine, unorderedBipartiteEdgeList))
weightedEmailNetworkEdgeTextList = list(map(writeAsCSVLineWeighted, weightedEmailNetworkEdgeList))

#Append headers to lists for printing.
networkEdgeListHeader = "To,From,weight\n"
bipartiteEdgeListHeader = "To,From\n"
unorderedBipartiteEdgeListHeader = "Address,email\n"

weightedEmailNetworkEdgeTextList = [networkEdgeListHeader] + weightedEmailNetworkEdgeTextList               #type is str[]
bipartiteEdgeTextList = [bipartiteEdgeListHeader] + bipartiteEdgeTextList                                   #type is str[]
unorderedBipartiteEdgeTextList = [unorderedBipartiteEdgeListHeader] + unorderedBipartiteEdgeTextList        #type is str[]


#Write all files
bipartiteEdgeListCsv.writelines(bipartiteEdgeTextList)
unorderedBipartiteEdgeListCsv.writelines(unorderedBipartiteEdgeTextList)
emailAttributesCsv.writelines(emailAttributesList)
emailNetworkEdgeCsv.writelines(weightedEmailNetworkEdgeTextList)
unreadableEmailsCsv.writelines(unreadableEmailsList)

print("Time elapsed(in seconds): " + str(time.time()-start))