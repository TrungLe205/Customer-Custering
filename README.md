# Customer Custering
Using Transaction Data of Ta-Feng supermarket from 2010 to 2011 to explore clustering and customer segmentation

In this project, we focused on practicing cleaning and manipulating data using “data.table” packages, creating 40 new features. Also, we applied PCA and k-mean clustering to segment customers.

# Dataset
### Data Discription
D11: Transaction data collected in November, 2000

D12: Transaction data collected in December, 2000

D01: Transaction data collected in January, 2001

D02: Transaction data collected in February, 2001

### Column definition

1: Transaction date and time (time invalid and useless)

2: Customer ID

3: Age: 10 possible values,

    A <25,B 25-29,C 30-34,D 35-39,E 40-44,F 45-49,G 50-54,H 55-59,I 60-64,J >65
    
    actually upon inspection there's 22362 rows with value K, will assume it's J+
    
4: Residence Area: 8 possible values, 

    A-F: zipcode area: 105,106,110,114,115,221,G: others, H: Unknown
    
    Distance to store, from the closest: 115,221,114,105,106,110
    
    "E","F","D","A","B","C","G","H"

5: Product subclass

6: Product ID

7: Amount

8: Asset

9: Sales price
