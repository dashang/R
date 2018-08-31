# R


Price of a property is one of the most important decision criterion when people buy homes. Real state firms need to be consistent in their pricing in order to attract buyers . Having a predictive model for the same will be great tool to have , which in turn can also be used to tweak development of properties , putting more emphasis on qualities which increase the value of the property.


There are two data Sets:
1) housing_train.csv 
2) housing_test.csv  
          We need to use data housing_train to build predictive model for response variable "Price". Housing_test data contains all other factors except "Price", we need to predict that using the model that you developed 

DATA DICTIONARY :
Each row represnts characteristic of a single property . Many categorical data has 
been coded to mask the data, you dont need to worry about their exact meaning 


Suburb : categorical :: Which subsurb the property is located in 

Address : categorical :: short address

Rooms : numeric :: Number of Rooms

Type : categorical :: type of the property

Price : numeric :: This is the target variable, price of the property 

Method : categorical :: method for selling 

SellerG : categorical :: Name of the seller 

Distance : numeric :: distance from the city center

Postcode : categorical :: postcode of the property

Bedroom2 : Numeric :: numbers of secondary bedrooms (this is different from rooms)

Bathroom : numeric :: number of bathrooms

Car : numeric :: number of parking spaces

Landsize : numeric :: landsize

BuildingArea : numeric :: buildup area

YearBuilt : numeric :: year of building 

CouncilArea : numeric :: council area to which the propery belongs
