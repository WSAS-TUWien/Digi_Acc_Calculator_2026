###############################################################
# ERP MAPPER
###############################################################

# Map '.erp' tables from ERP system to '.tbl' table that are aligned with ITEM.tbl
# Inputs: ITEM.tbl, BOM.erp, ROUTING.erp, MAT.erp
# Output: BOM.tbl, ROUTING.tbl, MAT.tbl

mapERPtoITEM <- function(ITEM.tbl, BOM.erp, ROUTING.erp, MAT.erp) {
  BOM.tbl <- BOM.erp |>
    left_join(
      ITEM.tbl |> select(parItemID = itemID, parCat = erpCat, parID = erpID),
      join_by(parCat, parID)) |>
    left_join(
      ITEM.tbl |> select(childItemID = itemID, childCat = erpCat, childID = erpID),
      join_by(childCat, childID)) |>
    select(bomID, parItemID, childItemID, rM)
  ROUTING.tbl <- ROUTING.erp |>
    left_join(
      ITEM.tbl |> select(itemID, prodType = erpCat, prodID = erpID),
      join_by(semFinCat == prodType, semFinID == prodID)) |>
    select(routeID, itemID, actyID, rE, eqipID, powE)
  MAT.tbl <- MAT.erp |>
    left_join(
      ITEM.tbl |> filter(erpCat == "mat") |> select(itemID, matID = erpID),
      join_by(matID)) |>
    select(itemID, pM, uMEL)
  result <- list(BOM.tbl = BOM.tbl, ROUTING.tbl = ROUTING.tbl, MAT.tbl = MAT.tbl)
  list2env(result, envir = .GlobalEnv)
  return(invisible(result))}

