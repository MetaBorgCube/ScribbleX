all: Seller.idr Customer.idr Bank.idr csb_S.idr csb_B.idr csb_C.idr Boot.idr
	idris Bank.idr -o bank --package contrib &&\
	idris Seller.idr -o seller --package contrib &&\
	idris Customer.idr -o customer --package contrib
