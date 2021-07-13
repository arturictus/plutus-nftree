
state = initialState "owner" [(1, "ipfs://")]

addTokenApproval state 1 "pub"

addOperatorApproval state "pub1" "pub2"
