###############################################################
# AIS ANALYZER
###############################################################

# Analysis of cost & GHG results retrieved from AIS.tbl
# Inputs: nodeIDs, AIS.tbl
# Output: nodeIDs' cost structures and GHG structures from different perspectives 

# Building data for multiple items
buildMultiItemCatShareData <- function(nodeIDs, AIS.tbl) {
  nodeAccData <- AIS.tbl |> filter(nodeID %in% nodeIDs)
  if (nrow(nodeAccData) == 0) {stop("No matching nodeIDs found in AIS.tbl.")}
  NodesCostEmi.tbl <- map_dfr(1:nrow(nodeAccData), function(i) {
    nodeID    <- nodeAccData$nodeID[i]
    nodeComps <- nodeAccData$nodeCompsList[[i]]
    # Cost shares
    nodeCost <- nodeComps |> group_by(costCat) |>
      summarise(cost  = sum(cost, na.rm = TRUE)) |>
      mutate(percent  = cost / sum(cost),
             share    = "Cost",
             category = as.character(costCat),
             nodeID   = paste0(nodeID)) |>
      select(nodeID, category, share, percent)
    # Emission shares
    nodeEmi <- nodeComps |> group_by(scopeCat) |>
      summarise(emi   = sum(emi, na.rm = TRUE)) |>
      mutate(percent  = emi / sum(emi),
             share    = "Emission",
             category = as.character(scopeCat),
             nodeID   = paste0(nodeID)) |>
      select(nodeID, category, share, percent)
    bind_rows(nodeCost, nodeEmi)})
  # Return data
  return(NodesCostEmi.tbl)}


# Plotting nodes combined
plotMultiItemCatSharesData <- function(NodesCostEmi.tbl) {
  ggplot(NodesCostEmi.tbl, aes(x = share, y = percent, fill = category)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ paste0("itemID", nodeID)) +
    scale_y_continuous(labels = function(x) paste0(x * 100, "%"),
                       sec.axis = sec_axis(~ ., name   = "Emission share (%)",
                                           labels = function(x) paste0(x * 100, "%"))) +
    scale_fill_brewer(palette = "Set2", name = "Category / Scope") +
    labs(x = "", y = "Cost share (%)",
         title = "Cost Categories and Emission Scopes for FPs",
         subtitle = "Dual axis, stacked percentage comparison") +
    theme_minimal(base_size = 14)}
