       >>SOURCE FORMAT FREE
*> Adam Buerger
*> CSC 407
*> Programming Assignment 2
*> Takes in financial information from three files 
*> (inventory information, customer information, and transaction information)
*> and generates reports about who ordered what, how much of a certain item
*> needs to be reordered to have a healthy inventory, and any errors or
*> discrepencies in transactions that would cause them to not be processed.
*> because of how the files are formatted, there are a sizable number of empty
*> spaces that have to be read in but are not are not relevant to the program. 
*> These empty spaces are placed in the buffer variables so the file can 
*> properly read other data. Records for writing to files also have buffers
*> so someone observing the files can properly read the information in the file.
*> It is worth noting that in Errors.dat, the ErrorType is a single character.
*> ErrorType will only be either a P to express that an error is caused by the
*> ItemID of the transaction or a C to express that an error is caused by the
*> CustomerID of the transaction.
IDENTIFICATION DIVISION.
PROGRAM-ID. Program2.
AUTHOR. Adam Buerger.
DATE-WRITTEN. February 11th 2021
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       *> create a file reader to view customer information
       SELECT CustomerFile ASSIGN TO "customers.dat"
           Organization is line sequential.
       *> create a file reader to view transaction information
       SELECT TransactionFile ASSIGN TO "transactions.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       *> create a file reader to view inventory information
       SELECT InventoryFile ASSIGN TO "inventory.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       *> create a file to write error reports to
       SELECT ErrorFile ASSIGN TO "Errors.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       *> create a file to write orders to
       SELECT InventoryOrder ASSIGN TO "InventoryOrder.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       *> create a file to write completed transactions to
       SELECT TransactionProcess ASSIGN TO "TransactionsProcessed.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
DATA DIVISION.
FILE SECTION.
*> create a record to read in transactions which contains
FD TransactionFile.
01 Transaction.
       *> the CustomerId for who order the item
       02 CustomerID PIC 99999.
       02 Buffer1 PIC X(5).
       *> the ItemID of what they're ordering
       02 ItemID PIC 999999.
       02 Buffer2 PIC X(6).
       *> the quantity of the item they're ordering
       02 Amount PIC 9.
       02 Buffer3 PIC X(5).
       *> a single character that represents any discount they may be receiving on the order
       02 DiscountCode PIC A.
*> create a record to more easily write information to ErrorFile which contains
FD ErrorFile.
01 ErrorData.
       *> the potentially incorrect Customer ID
       02 CustomerID PIC 99999.
       02 Buffer1 PIC XXXX.
       *> the potentially incorrect Item ID
       02 ItemID PIC 999999.
       02 Buffer2 PIC XXXX.
       *> the amount of the item ordered
       02 Amount PIC 9.
       02 Buffer3 PIC XXXX.
       *> which ID is wrong
       02 ErrorType PIC A.
*> create a record to more easily write inventory orders which contains
FD InventoryOrder.
01 OrderData.
       *> the item ID for what needs to be ordered
       02 ItemID PIC 999999.
       02 Buffer PIC XXXX.
       *> the amount that needs to be ordered
       02 Amount PIC 9(2).
*> create a record to hold processed transaction information which contains
FD TransactionProcess.
01 ProcessedTransaction.
       *> Customer's name
       02 CustomerName PIC A(18).
       *> customer address which contains
       02 CustomerAddress PIC X(48).
       *> the item name
       02 ItemName PIC X(24).
       *> the amount of the item ordered
       02 Amount PIC 9.
       02 Buffer1 PIC XXXX.
       *> the cost of an individual item multiplied by the amount ordered
       02 GrossCost PIC 999.99.
       02 Buffer2 PIC XXXX.
       *> the amount discounted
       02 Discount PIC 999.99.
       02 Buffer3 PIC XXXX.
       *> the gross cost minus the discount
       02 NetCost PIC 999.99.
       02 Buffer4 PIC XXXX.
       *> the amount the customer now owes
       02 Owed PIC 999.99.
*> create a record for what needs to be read in from CustomerFile
FD CustomerFile.
*> the record contains all information on a customer given in the file including
01 TempCustomer.   
       *> the customer's ID number
       02 CustomerID PIC 999999.
       02 Buffer1 PIC X(5).
       *> the customer's first and last name
       02 CustomerName PIC X(23).
       *> the customer's address (their street address, the city they live in, and the state/country they live in)
       02 CustomerAddress PIC X(48).
       *> and how much they owe from their previous order
       02 AmtOwed PIC 999V99.
*> create a record for what needs to be read in from InventoryFile
FD InventoryFile.
*> This record contains all information about an item in stock including
01 Item.
       *> its six digit ItemID
       02 ItemID PIC 999999.
       02 Buffer1 PIC X(5).
       *> the item name
       02 ItemName PIC X(25).
       *> how many items are currently in stock
       02 InStock PIC 99.
       02 Buffer2 PIC X(5).
       *> how many the store can have in stock before needing to reorder
       02 MinStock PIC 99.
       02 Buffer3 PIC X(5).
       *> the price of the item
       02 Price PIC 99.99.
WORKING-STORAGE SECTION.
*> create 24 instances of an inventory record which contains
01 Inventory OCCURS 24 TIMES.
       *> the item ID
       02 ItemID PIC 999999.
       02 Buffer1 PIC X(5).
       *> the item name
       02 ItemName PIC X(25).
       *> the amount of the item in stock
       02 InStock PIC 99.
       02 Buffer2 PIC X(5).
       *> the minimum amount the store could have before needing to order more
       02 MinStock PIC 99.
       02 Buffer3 PIC X(5).
       *> the price of an individual unit of those items
       02 Price PIC 99.99.
*> create 10 instances of a customer record which contains
01 Customer OCCURS 10 TIMES.
       *> the customer ID
       02 CustomerID PIC 99999.
       02 Buffer1 PIC X(5).
       *> the customer's name which contains
       02 CustomerName PIC X(23).
       *> the customer's address which contains
       02 CustomerAddress PiC X(48).
       *> the amount the customer owes the store
       02 AmtOwed PIC 999V99.
*> create a counter variable to cycle through the array of Customers
01 CustomerCount PIC 99 VALUE 1.
*> create a counter variable to cycle through the array of Items
01 InventoryCount PIC 99 Value 1.
*> create a "boolean" value to store whether or not you found the CustomerID in a transaction
01 FoundCustomer Pic A value 'N'.
*> create a "boolean" value to store whether or not you found the ItemID in a transaction
01 FoundItem Pic A value 'N'.
*> create a variable to store an index of a needed item in the Inventory array
01 StoredItem Pic 99.
*> create a variable to store an index of a needed Customer in the Customer array
01 StoredCustomer Pic 99.
*> create temporary decimals to perform calculations on
01 tempPrice Pic 999v99.
01 tempGross Pic 999v99.
01 tempNet Pic 999v99.
01 tempDiscount Pic 999v99.
PROCEDURE DIVISION.
Perform ReadCustomer. *> read in the customer data
Perform ReadInventory. *> read in the inventory data
Open input TransactionFile.
open output TransactionProcess
open output ErrorFile
open output InventoryOrder
   perform 21 times
       Perform ResetVariables *> assume that the next transaction is invlad
       *> read in the next transaction
       Read TransactionFile
       Perform FindCustomer *> search for CustomerID in Transaction in the CusomerArray
       Perform FindItem *> search for the ItemID in Transaction
       *> if the CustomerID is invalid
       if FoundCustomer = 'N' or FoundItem = 'N' then    
           Perform WriteError *> write the error data to Errors.dat
       else
           Perform AttemptOrder *> see if an inventory order needs to be placed
           Perform InputTransactionInfo *> move Customer and Item information into ProcessedTransaction
           Perform ComputePrices *> find the net and gross costs as well as the discount for the transaction
           Write ProcessedTransaction
       end-if
   end-perform
close InventoryOrder
close ErrorFile
close TransactionProcess
Close TransactionFile.
STOP RUN.
*> reads in all customer data into the Customer array
ReadCustomer.
   OPEN INPUT CustomerFile.
       Perform until CustomerCount = 11
           *> read in all ten customers from customers.dat
           Read CustomerFile into Customer(CustomerCount)
           *> incriment CustomerCount
           NOT AT END add 1 to CustomerCount
           END-READ
       End-perform
   Close CustomerFile.
*> reads in all inventory data into the Inventory array
ReadInventory.
    OPEN INPUT InventoryFile.
       Perform until InventoryCount = 25
           *> read in all 24 inventory items from inventory.dat
           Read InventoryFile into Inventory(InventoryCount)
           *> incriment InventoryCount
           Not at end add 1 to InventoryCount
           end-read
       end-perform
   close InventoryFile.
*> resets the counters and found flags to start at the beginning of the array and assume a given transaction is invalid until proven otherwise
ResetVariables.
   *> reset CustomerCount to check the next transaction
   Set CustomerCount to 1.
   *> assume the CustomerID in the next transaction does not exist
   Move 'N' to FoundCustomer.
   *> reset the InventoryCount to check the next transaction
   Set InventoryCount to 1.
   *> assume the ItemID in the next transaction does not exist
   Move 'N' to FoundItem.
*> attempts to find the CustomerID of Transaction in the CUstomer array
FindCustomer.
    perform 11 times
       *> check the CustomerID in the transaction against all CustomerIDs
       if CustomerID in Transaction = CustomerID in Customer(CustomerCount) then
           *> mark that the CustomerID has been found if it exists
           Move 'Y' to FoundCustomer
           *> store the index the found customer is at
           Move CustomerCount to StoredCustomer
       end-if
       *> incriment CustomerCount
       Add 1 to CustomerCount
    end-perform.
*> attempts to find the ItemID of Transaction in the Inventory array
FindItem.
    perform 25 times
       *> check the ItemID in the transaction against all ItemIDs 
       if ItemID in Transaction = ItemId in Inventory(InventoryCount)
           *> mark that the ItemID has been found if it exists
           Move 'Y' to FoundItem
           *> store the index the cound item is at
           Move InventoryCount to StoredItem
       end-if
       *> incriment InventoryCount
       Add 1 to InventoryCount
    end-perform.
*> Writes a found error to Errors.dat
WriteError.
    *> move CustomerID, ItemID, and Amount into the ErrorData record
       move CustomerID in Transaction to CustomerID in ErrorData.
       move ItemID in Transaction to ItemID in ErrorData.
       move Amount in Transaction to Amount in ErrorData.
       if FoundCustomer = 'N' then
           *> move C into the ErrorData record to show that the error is the result of an invalid CustomerID
           move 'C' to ErrorType in ErrorData
       end-if.
       *> if the ItemID is invalid
       if FoundItem = 'N' then
           *> move P into the ErrorData record to show that the error is the result of an invalid ItemID
           move 'P' to ErrorType in ErrorData
       end-if.
       Write ErrorData.
*> finds the discount for a given order based on the DiscountCode in Transaction
FindDiscount.
    Evaluate DiscountCode
       When 'A' *> 10% off
           Compute tempDiscount = tempGross * 0.1
       When 'B' *> 20% off
           Compute tempDiscount = tempGross * 0.2
       When 'C' *> 25% off
           Compute tempDiscount = tempGross * 0.25
       When 'D' *> buy three or more and get one free
           Move Price in Inventory(StoredItem) to tempDiscount
       When 'E' *> buy 1 get 1 free
           Compute tempDiscount = tempGross * 0.5
       When 'Z' *> no discount
           Move 0 to tempDiscount
    end-evaluate.
    Move tempDiscount to Discount.
*> determines if an inventory order needs to be placed and places it if necessary
AttemptOrder.
    *> find the amount of stock after the transaction is placed
    Subtract Amount in Transaction from InStock in Inventory(StoredItem).
    *> find the amount that needs to be ordered based on the MinStock and what the current sock is
    *> if min stock is 1
    if MinStock in Inventory(StoredItem) = 1
       *> order enough to have 3
       Compute Amount in OrderData = 3 - InStock in Inventory(StoredItem)
    else 
       *> if min stock is between 2 and 5
       if MinStock in Inventory(StoredItem) >= 2 and MinStock in Inventory(StoredItem) <= 5
           *> order enough to have 6
           Compute Amount in OrderData = 6 - InStock in Inventory(StoredItem)
       else 
           *> if min stock is between 6 and 10
           if MinStock in Inventory(StoredItem) >= 6 and MinStock in Inventory(StoredItem) <= 10
               *> order enough to have 12
               Compute Amount in OrderData = 12 - InStock in Inventory(StoredItem)
           else
               *> if min stock is between 11 and 20
               if MinStock in Inventory(StoredItem) >= 11 and MinStock in Inventory(StoredItem) <= 20
                   *> order enough to have 25
                   Compute Amount in OrderData = 25 - InStock in Inventory(StoredItem)
               else 
                   *> otherwise order enough to have 30
                   Compute Amount in OrderData = 30 - InStock in Inventory(StoredItem)
           end-if
       end-if
   end-if.
   *> add the ordered stock to the store stock
   Add Amount in OrderData to InStock in Inventory(StoredItem).
   *> move the ItemID of the ordered item into OrderData
   Move ItemID in Inventory(StoredItem) to ItemID in OrderData.
   *> if you need to order anything, write the order to InventoryOrder.dat
   if Amount in OrderData > 0
       write OrderData
   end-if.
InputTransactionInfo.
    *> move the found CustomerName and CustoemrAddress into the processed transaction
    Move CustomerName in Customer(StoredCustomer) to CustomerName in ProcessedTransaction.
    Move CustomerAddress in Customer(StoredCustomer) to CustomerAddress in ProcessedTransaction.
    *> move the found ItemName into the proccessed transaction
    Move ItemName in Inventory(StoredItem) to ItemName in ProcessedTransaction.
    *> move the amount of items ordered into the processed transaction
    Move Amount in Transaction to Amount in ProcessedTransaction.
    *> move the price of the item into a temporary variable to do arithmetic in
    Move Price in Inventory(StoredItem) to tempPrice.
*> computes the GrossCost, NetCost, Discount, and Owed for the processed transaction
ComputePrices.
    *> calculate the gross cost of the transaction by multiplying the price by the amount ordered
    Compute tempGross =  tempPrice * Amount in Transaction.
    Move tempGross to GrossCost.
    *> decide discount based on the value of DiscountCode
    Perform FindDiscount.
    *> calculate the net cost by subtracting the discount from the gross cost
    Compute tempNet = tempGross - tempDiscount.
    Move tempNet to NetCost.
    *> determine how much the customer now owes
    Add tempNet to AmtOwed in Customer(StoredCustomer).
    *> store the new amclearount owed in the processed transaction
    Move AmtOwed in Customer(StoredCustomer) to Owed.
