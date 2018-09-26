.DEFAULT_GOAL := all

bank: Bank.idr csb_B.idr Boot.idr
	idris Bank.idr -o bank --package contrib

seller: Seller.idr csb_S.idr Boot.idr
	idris Seller.idr -o seller --package contrib

customer: Customer.idr csb_C.idr Boot.idr
	idris Customer.idr -o customer --package contrib

all: bank seller customer
