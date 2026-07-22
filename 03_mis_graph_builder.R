###############################################################
# MIS GRAPH BUILDER
###############################################################

# Build Production Information System (MIS) table
# Inputs: ITEM.tbl, ROUTING.tbl, ACTY.erp, BOM.tbl, EQIP.erp, ENERGY.erp
# Output: MIS.tbl with nodeID, chilNodeIDs, etc.

buildMIS <- function(ITEM.tbl, ROUTING.tbl, ACTY.tbl, BOM.tbl, EQIP.tbl, ENERGY.tbl) {
  # 1) Base MIS: join routing with activities and items
  MIS <- ROUTING.tbl |>
    left_join(ACTY.tbl, join_by(actyID)) |>
    left_join(ITEM.tbl |> select(itemID, itemName, itemCat), join_by(itemID))
  if (nrow(MIS) == 0) {
    stop("ROUTING.tbl produced no rows — check ITEM.tbl mapping.")}
  # 2) Normalize eqipID and powE into list-columns
  MIS <- MIS |>
    mutate(eqipID = map(eqipID, ~ {
      if (is.null(.x) || all(is.na(.x))) integer() else as.integer(.x)}),
      powE = map(powE, ~ {
        if (is.null(.x) || all(is.na(.x))) numeric() else as.numeric(.x)}))
  # 3) Pairwise explosion of equipment and power
  MIS <- MIS |>
    mutate(eqipPow = map2(eqipID, powE, ~ tibble(eqipID = .x, powE = .y))) |>
    select(-eqipID, -powE) |>
    unnest(eqipPow, keep_empty = TRUE)
  # 4) Join EQIP + ENERGY and compute powEmE by scope category
  MIS <- MIS |>
    left_join(EQIP.tbl, join_by(eqipID)) |>
    left_join(ENERGY.tbl, join_by(enyID)) |>
    mutate(powEmE01 = if_else(!is.na(powE) & scopeCat == 1, powE * emE, 0),
           powEmE02 = if_else(!is.na(powE) & scopeCat == 2, powE * emE, 0))
  # 5) Aggregate equipment into eqipList and compute uACF
  MIS <- MIS |>
    group_by(routeID, itemID, itemName, itemCat, actyID, actyName, rE, pA) |>
    summarise(eqipList = list(
      tibble(eqipID   = eqipID,   eqipName = eqipName, qE       = qE,
             cuEEL    = cuEEL,    enyID    = enyID,    emE      = emE, 
             scopeCat = scopeCat, powE     = powE,     powEmE01 = powEmE01,
             powEmE02 = powEmE02)),
      uACF01 = sum(powEmE01, na.rm = TRUE),
      uACF02 = sum(powEmE02, na.rm = TRUE), .groups = "drop")
  # 6) Add matList from BOM
  MIS <- MIS |> left_join( BOM.tbl |> group_by(parItemID) |>
                             summarise(matList = list(tibble(childItemID, rM)), .groups = "drop"),
                           join_by(itemID == parItemID)) |>
    mutate(matList = map(matList, ~ {
      if (is.null(.x)) tibble(childItemID = integer(), rM = numeric()) else .x}))
  # 7) Assign nodeID in ITEM.tbl top‑down order
  MIS <- MIS |> arrange(itemID, routeID) |> mutate(nodeID = row_number())
  # 8) Compute chilNodeIDs (children = intermediate items)
  lookup <- MIS |> select(itemID, nodeID)
  MIS <- MIS |>
    mutate(chilNodeIDs = map(matList, ~ {
      childItemIDs <- .x$childItemID
      intItemIDs <- ITEM.tbl |> 
        filter(itemID %in% childItemIDs, itemCat == "intermediate") |> pull(itemID)
      lookup |> filter(itemID %in% intItemIDs) |> pull(nodeID)}))
  MIS.tbl <- MIS
  # 9) Return data
  return(MIS.tbl)}

