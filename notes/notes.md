### Unisg market place contract
- Offer , Bid and Sell
- The datum will contain reference to the seller or buyer
- NFTs to identify the bid utxo   
- Owner 1 and owner 2 seems to be marketplace owners


### Questions about space buds contract:
- The need to identify the relevant utxos arise as there is no restriction to creation of utxo at script address. To achieve that, is using NFT as identifier the best approach? 
- How do the endpoints get invoked in the spacebuds via serialisation libs? 
- How does TradeDetails get used in space buds via serialisation libs? 
- The tradeValidator is not parametrised. So there is one script address in case of space buds. Is that necessary for querying? How do they query assets at the script 
- The contract follows the older version of Plutus. So how are they running this in production? 