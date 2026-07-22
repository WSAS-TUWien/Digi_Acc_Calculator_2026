###############################################################
# ACC RECURSION ENGINE
###############################################################

# Recursive calculation of cost & GHG result table (Result.tbl)
# Inputs: nodeIDs, PIS.tbl, ITEM.tbl, MAT.tbl
# Output: nodeIDs' Product Cost (PC) and Product Carbon Footprint (PCF) and 
#         children's contributions

# Single nodeID calculation
calcAccAttr <- function(nodeID, PIS.tbl, ITEM.tbl, MAT.tbl) {
  node <- PIS.tbl |> filter(nodeID == !!nodeID)
  # Activity and equipment
  pA       <- node$pA
  uACF01   <- node$uACF01
  uACF02   <- node$uACF02
  eqipList <- node$eqipList[[1]]
  # Split equipment cost qE by scope
  qE01 <- sum(eqipList$qE[eqipList$scopeCat == 1], na.rm = TRUE)
  qE02 <- sum(eqipList$qE[eqipList$scopeCat == 2], na.rm = TRUE)
  # Activity overhead (labour etc.) – not split
  AOC  <- node$rE * pA
  # Equipment overhead – split by scope
  AOC01 <- node$rE * qE01
  AOC02 <- node$rE * qE02
  # Assign AOC (pA part) to the scope that exists
  if (qE01 > 0 & qE02 == 0) {
    AOC01 <- AOC01 + AOC} 
  else if (qE02 > 0 & qE01 == 0) {
    AOC02 <- AOC02 + AOC} 
  else if (qE01 > 0 & qE02 > 0) {
    total_qE <- qE01 + qE02
    AOC01 <- AOC01 + AOC * (qE01 / total_qE)
    AOC02 <- AOC02 + AOC * (qE02 / total_qE)} 
  else {AOC02 <- AOC}
  AECF01 <- node$rE * uACF01
  AECF02 <- node$rE * uACF02
  uEEL   <- sum(eqipList$cuEEL, na.rm = TRUE)
  AEEL   <- node$rE * uEEL
  # Materials
  matList <- node$matList[[1]]
  if (nrow(matList) > 0) {
    matInputs <- matList |>
      left_join(ITEM.tbl |> select(itemID, itemCat), join_by(childItemID == itemID)) |>
      filter(itemCat == "material") |>
      inner_join(MAT.tbl, join_by(childItemID == itemID)) |>
      mutate(cost = pM * rM,
             emi  = uMEL * rM)
    AMC  <- sum(matInputs$cost, na.rm = TRUE)
    AMCF <- sum(matInputs$emi,  na.rm = TRUE)} 
  else {AMC  <- 0
  AMCF <- 0}
  # Assigning attributes and totals to nodes (core of node-based accounting)
  nodeAttr <- bind_rows(
    tibble(nodeID = node$nodeID, nodeComp = "ACTY S1",  cost     = AOC01,
           emi    = AECF01,      costCat  = "indirect", scopeCat = 1),
    tibble(nodeID = node$nodeID, nodeComp = "ACTY S2",  cost     = AOC02,
           emi    = AECF02,      costCat  = "indirect", scopeCat = 2),
    tibble(nodeID = node$nodeID, nodeComp = "EQIP",     cost     = AEEL,
           emi    = AEEL,        costCat  = "indirect", scopeCat = 3),
    tibble(nodeID = node$nodeID, nodeComp = "MAT",      cost     = AMC,
           emi    = AMCF,        costCat  = "direct",   scopeCat = 3))
  nodeTotal <- tibble(nodeID  = node$nodeID, 
                      nodePC  = sum(nodeAttr$cost),
                      nodePCF = sum(nodeAttr$emi))
  # Recursion
  chilNodeIDs <- node$chilNodeIDs[[1]]
  # Base case
  if (length(chilNodeIDs) == 0) {
    return(list(nodeAttr = nodeAttr,
                nodeTotals = nodeTotal))}
  # Recursion case
  chilAttr <- map(chilNodeIDs, ~ calcAccAttr(.x, PIS.tbl, ITEM.tbl, MAT.tbl))
  # Return data
  NodeAccTree.list <- 
    list(nodeAttrs  = bind_rows(nodeAttr, map(chilAttr, "nodeAttr") |> bind_rows()),
         nodeTotals = bind_rows(nodeTotal, map(chilAttr, "nodeTotals") |> bind_rows()))
  return(NodeAccTree.list)}


# Calculating accounting results for multiple nodeIDs
buildMultiItemAccData <- function(nodeIDs, PIS.tbl, ITEM.tbl, MAT.tbl) {
  map_dfr(nodeIDs, function(nodeID) {
    nodeAccTree <- calcAccAttr(nodeID, PIS.tbl, ITEM.tbl, MAT.tbl)
    # Components for this subtree 
    nodeComps <- if (!is.null(nodeAccTree$nodeAttrs)) {nodeAccTree$nodeAttrs } # int. node
    else {nodeAccTree$nodeAttr}                                               # leaf node
    # Totals for this subtree (node + all children)
    nodeTotals <- nodeAccTree$nodeTotals
    # Return data
    MultiNodeAcc.tbl <- 
      tibble(nodeID         = nodeID,
             PC             = sum(nodeTotals$nodePC),
             PCF            = sum(nodeTotals$nodePCF),
             nodeCompsList  = list(nodeComps),
             nodeTotalsList = list(nodeTotals))
    return(MultiNodeAcc.tbl)})}
