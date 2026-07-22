###############################################################
# UNIFIED ITEM MASTER (ITEM.tbl)
###############################################################

# Build unified item master table
# Inputs: MAT.erp, SEMI.erp, FINAL.erp
# Output: ITEM.tbl with itemID, itemType, etc.

buildItemMaster <- function(MAT.tbl, SEMI.tbl, FINAL.tbl) {
  mpMat <- MAT.tbl |>
    transmute(erpID = matID,
              erpCat = "mat",
              itemName = matName,
              itemCat = "material")
  mpSemi <- SEMI.tbl |>
    transmute(erpID = spID,
              erpCat = "semi",
              itemName = spName,
              itemCat = "intermediate")
  mpFinal <- FINAL.tbl |>
    transmute(erpID = fpID,
              erpCat = "final",
              itemName = fpName,
              itemCat = "finished")
  Item.tbl <- bind_rows(mpFinal, mpSemi, mpMat) |>
    mutate(itemID = row_number()) |>
    select(itemID, erpID, erpCat, itemName, itemCat)
  return(Item.tbl)}

