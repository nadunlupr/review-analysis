source("helpers.R")
ensure_packages(); load_packages()

df_long <- expand_all_factors(read_lit())
net_df <- make_bipartite_factor_network(df_long, "Platform")
plot_bipartite_factor_network(net_df, dimension_label = "Platform", file_name = "network_platform_factor.png")
