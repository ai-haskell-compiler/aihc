{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Api.TestByteStrings where

import           Data.ByteString.Lazy (ByteString)
import           Text.RawString.QQ    (r)

responseAuthGet :: ByteString
responseAuthGet = [r|{"accounts":[{"account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","balances":{"available":100,"current":110,"limit":null,"iso_currency_code":"USD","unofficial_currency_code":null},"mask":"9606","name":"Plaid Checking","official_name":"Plaid Gold Checking","subtype":"checking","type":"depository"}],"numbers":{"ach":[{"account":"9900009606","account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","routing":"011401533","wire_routing":"021000021"}],"eft":[{"account":"111122223333","account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","institution":"021","branch":"01140"}],"international":[{"account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","bic":"NWBKGB21","iban":"GB29NWBK60161331926819"}],"bacs":[{"account":"31926819","account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","sort_code":"601613"}]},"item":{"available_products":["assets","balance","credit_details","identity","income","investments","liabilities"],"billed_products":["auth","transactions"],"error": null,"institution_id":"ins_3","item_id":"MylwooVBojuBwgzNVdZ6FDLRnElJN3C9WPd49","webhook":"http://bogicevicsasa.com"},"request_id":"m8MDnv9okwxFNBV"} |]

responseTransactionsGet :: ByteString
responseTransactionsGet = [r|{"accounts":[{"account_id":"vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d","balances":{"available":100,"current":110,"limit":null,"iso_currency_code":"USD","unofficial_currency_code":null},"mask":"9606","name":"Plaid Checking","official_name":"Plaid Gold Checking","subtype":"checking","type":"depository"}], "transactions": [{"account_id": "vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d", "amount": 2307.21, "iso_currency_code": "USD", "unofficial_currency_code": null, "category": ["Shops", "Computers and Electronics"], "category_id": "19013000", "date": "2017-01-29", "location": {"address": "300 Post St", "city": "San Francisco", "region": "CA", "postal_code": "94108", "country": "US", "lat": null, "lon": null}, "name": "Apple Store", "payment_meta": { "reference_number":null, "ppd_id":null, "payee":null}, "pending": false, "pending_transaction_id": null, "account_owner": null, "transaction_id": "lPNjeW1nR6CDn5okmGQ6hEpMo4lLNoSrzqDje", "transaction_type": "place"}, {"account_id": "XA96y1wW3xS7wKyEdbRzFkpZov6x1ohxMXwep", "amount": 78.5, "iso_currency_code": "USD", "unofficial_currency_code": null, "category": ["Food and Drink", "Restaurants"], "category_id": "13005000", "date": "2017-01-29", "location": {"address": "262 W 15th St", "city": "New York", "region": "NY", "postal_code": "10011", "country": "US", "lat": 40.740352, "lon": -74.001761}, "name": "Golden Crepes", "payment_meta": { "reference_number":null, "ppd_id":null, "payee":null}, "pending": false, "pending_transaction_id": null, "account_owner": null, "transaction_id": "4WPD9vV5A1cogJwyQ5kVFB3vPEmpXPS3qvjXQ", "transaction_type": "place"}], "item": {"available_products":["assets","balance","credit_details","identity","income","investments","liabilities"],"billed_products":["auth","transactions"],"error": null,"institution_id":"ins_3","item_id":"MylwooVBojuBwgzNVdZ6jFDLRnElJN3C9WPd49","webhook":"http://bogicevicsasa.com"}, "total_transactions": 100, "request_id": "45QSn"}|]

responsePublicTokenCreate :: ByteString
responsePublicTokenCreate = [r|
   {"public_token":"public-sandbox-b0e2c4ee-a763-4df5-bfe9-46a46bce993d","request_id":"Aim3b"}
|]

responsePublicTokenExchange :: ByteString
responsePublicTokenExchange = [r|
  {"access_token":"access-sandbox-de3ce8ef-33f8-452c-a685-8671031fc0f6","item_id":"M5eVJqLnv3tbzdngLDp9FL5OlDNxlNhlE55op","request_id":"Aim3b"}
|]

balanceJson :: ByteString
balanceJson = [r|{"available":100,"current":110,"limit":null,"iso_currency_code":"USD","unofficial_currency_code":null}|]

accountJson :: ByteString
accountJson = [r|{"account_id":"vzeNDwK7KQIm4yEog683uElbp9GRLEFXGK98D","balances":{"available":100,"current":110,"limit":null,"iso_currency_code":"USD","unofficial_currency_code":null},"mask":"9606","name":"Plaid Checking","official_name":"Plaid Gold Checking","subtype":"checking","type":"depository"}|]

accountListJson :: ByteString
accountListJson = [r|[{"account_id":"vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d","balances":{"available":100,"current":110,"limit":null,"iso_currency_code":"USD","unofficial_currency_code":null},"mask":"9606","name":"Plaid Checking","official_name":"Plaid Gold Checking","subtype":"checking","type":"depository"}]|]

paymentMetaJson :: ByteString
paymentMetaJson = [r|{"reference_number":null, "ppd_id":null, "payee":null}|]

itemJson :: ByteString
itemJson = [r|{"available_products": ["assets","balance","credit_details","identity","income","investments","liabilities"], "billed_products":["auth","transactions"], "error": null, "institution_id":"ins_3", "item_id":"MylwooVBojuBwgzNVdZ6jFDLRnElJN3C9WPd49", "webhook":"http://bogicevicsasa.com"}|]

transactionJson :: ByteString
transactionJson = [r|{"account_id": "vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d", "amount": 2307.21, "iso_currency_code": "USD", "unofficial_currency_code": null, "category": ["Shops", "Computers and Electronics"], "category_id": "19013000", "date": "2017-01-29", "location": {"address": "300 Post St", "city": "San Francisco", "region": "CA", "postal_code": "94108", "country": "US", "lat": null, "lon": null}, "name": "Apple Store", "payment_meta": { "reference_number":null, "ppd_id":null, "payee":null}, "pending": false, "pending_transaction_id": null, "account_owner": null, "transaction_id": "lPNjeW1nR6CDn5okmGQ6hEpMo4lLNoSrzqDje", "transaction_type": "place"}|]

transactionLocationJson :: ByteString
transactionLocationJson = [r|{"address": "300 Post St", "city": "San Francisco", "region": "CA", "postal_code": "94108", "country": "US", "lat": null, "lon": null}|]

emailJson :: ByteString
emailJson = [r|
  {
    "data": "accountholder0@example.com",
    "primary": true,
    "type": "primary"
  }
|]

phoneNumberJson :: ByteString
phoneNumberJson = [r|
  {
    "data": "1112223333",
    "primary": false,
    "type": "home"
  }
|]

addressesJson :: ByteString
addressesJson = [r|
  {
    "data": {
      "city": "Malakoff",
      "country": null,
      "postal_code": "14236",
      "region": "NY",
      "street": "2992 Cameron Road"
    },
    "primary": true
  }
|]

ownersJson :: ByteString
ownersJson = [r|
  {
    "addresses": [
      {
        "data": {
          "city": "Malakoff",
          "country": null,
          "postal_code": "14236",
          "region": "NY",
          "street": "2992 Cameron Road"
        },
        "primary": true
      },
      {
        "data": {
          "city": "San Matias",
          "country": null,
          "postal_code": "93405-2255",
          "region": "CA",
          "street": "2493 Leisure Lane"
        },
        "primary": false
      }
    ],
    "emails": [
      {
        "data": "accountholder0@example.com",
        "primary": true,
        "type": "primary"
      },
      {
        "data": "accountholder1@example.com",
        "primary": false,
        "type": "secondary"
      },
      {
        "data": "extraordinarily.long.email.username.123",
        "primary": false,
        "type": "other"
      }
    ],
    "names": [
      "Alberta Bobbeth Charleson"
    ],
    "phone_numbers": [
      {
        "data": "1112223333",
        "primary": false,
        "type": "home"
      },
      {
        "data": "1112224444",
        "primary": false,
        "type": "work"
      },
      {
        "data": "1112225555",
        "primary": false,
        "type": "mobile1"
      }
    ]
  }
|]

accountsJson :: ByteString
accountsJson = [r|
{
     "account_id": "9xPwwnJGpDI8Qby6ElmEuk1jGDmjlqcRWwLAz",
     "balances": {
       "available": 100,
       "current": 110,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "0000",
     "name": "Plaid Checking",
     "official_name": "Plaid Gold Standard 0% Interest Checki",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "checking",
     "type": "depository"
   }
|]

identityJson :: ByteString
identityJson = [r|
{
 "accounts": [
   {
     "account_id": "9xPwwnJGpDI8Qby6ElmEuk1jGDmjlqcRWwLAz",
     "balances": {
       "available": 100,
       "current": 110,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "0000",
     "name": "Plaid Checking",
     "official_name": "Plaid Gold Standard 0% Interest Checki",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "checking",
     "type": "depository"
   },
   {
     "account_id": "v4xRRVvpzyTlBMpaDjQDFWAGVXNGadSW3BjNo",
     "balances": {
       "available": 200,
       "current": 210,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "1111",
     "name": "Plaid Saving",
     "official_name": "Plaid Silver Standard 0.1% Interest Sa",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "savings",
     "type": "depository"
   },
   {
     "account_id": "Rrz11gqRL6SVljzWGJkGI7apezqpJbhRvWKDR",
     "balances": {
       "available": null,
       "current": 1000,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "2222",
     "name": "Plaid CD",
     "official_name": "Plaid Bronze Standard 0.2% Interest CD",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "cd",
     "type": "depository"
   },
   {
     "account_id": "6zaBBWpoM5HJqEkVy5zyUeML9xRL8kigopWN8",
     "balances": {
       "available": null,
       "current": 410,
       "iso_currency_code": "USD",
       "limit": 2000,
       "unofficial_currency_code": null
     },
     "mask": "3333",
     "name": "Plaid Credit Card",
     "official_name": "Plaid Diamond 12.5% APR Interest Credi",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "credit card",
     "type": "credit"
   },
   {
     "account_id": "Xa4wwnxzl6IQWD4eKBvKFwjeG4XeBLHdWN4V1",
     "balances": {
       "available": 43200,
       "current": 43200,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "4444",
     "name": "Plaid Money Market",
     "official_name": "Plaid Platinum Standard 1.85% Interest",
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "money market",
     "type": "depository"
   },
   {
     "account_id": "DMgwwaJx36SdwzmkaNMaSlxb56PbNXtvPBgVE",
     "balances": {
       "available": null,
       "current": 320.76,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "5555",
     "name": "Plaid IRA",
     "official_name": null,
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "ira",
     "type": "investment"
   },
   {
     "account_id": "VxdggABl86IamVdJNXkNuMdEDnVEBWHW8X4xn",
     "balances": {
       "available": null,
       "current": 23631.9805,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "6666",
     "name": "Plaid 401k",
     "official_name": null,
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "401k",
     "type": "investment"
   },
   {
     "account_id": "wNd55VvgBwSRXJEP67k6Unl9wxJ9jvFr7xvkL",
     "balances": {
       "available": null,
       "current": 65262,
       "iso_currency_code": "USD",
       "limit": null,
       "unofficial_currency_code": null
     },
     "mask": "7777",
     "name": "Plaid Student Loan",
     "official_name": null,
     "owners": [
       {
         "addresses": [
           {
             "data": {
               "city": "Malakoff",
               "country": null,
               "postal_code": "14236",
               "region": "NY",
               "street": "2992 Cameron Road"
             },
             "primary": true
           },
           {
             "data": {
               "city": "San Matias",
               "country": null,
               "postal_code": "93405-2255",
               "region": "CA",
               "street": "2493 Leisure Lane"
             },
             "primary": false
           }
         ],
         "emails": [
           {
             "data": "accountholder0@example.com",
             "primary": true,
             "type": "primary"
           },
           {
             "data": "accountholder1@example.com",
             "primary": false,
             "type": "secondary"
           },
           {
             "data": "extraordinarily.long.email.username.123",
             "primary": false,
             "type": "other"
           }
         ],
         "names": [
           "Alberta Bobbeth Charleson"
         ],
         "phone_numbers": [
           {
             "data": "1112223333",
             "primary": false,
             "type": "home"
           },
           {
             "data": "1112224444",
             "primary": false,
             "type": "work"
           },
           {
             "data": "1112225555",
             "primary": false,
             "type": "mobile1"
           }
         ]
       }
     ],
     "subtype": "student",
     "type": "loan"
   }
 ],
 "item": {
   "available_products": [
     "assets",
     "auth",
     "balance",
     "credit_details",
     "income",
     "investments",
     "liabilities"
   ],
   "billed_products": [
     "identity",
     "transactions"
   ],
   "consent_expiration_time": null,
   "error": null,
   "institution_id": "ins_3",
   "item_id": "Wda44G3KEVsz9ValeLmeh14nVAKLJwhlG8XN3",
   "webhook": ""
 },
 "request_id": "Mjv0DF1S06RqIuT"
}
|]

incomeJson :: ByteString
incomeJson = [r|

|]
