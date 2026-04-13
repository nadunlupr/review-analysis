# ============================================================
# AV Passenger Experience Design Space – Interactive Sankey
# ============================================================

packages <- c("networkD3", "dplyr", "purrr", "stringr", "janitor", "htmlwidgets", "jsonlite")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 2 Clean platform names
# ============================================================

data$platform <- tolower(data$platform)

data$platform <- case_when(
  str_detect(data$platform,"sim") ~ "Simulator",
  str_detect(data$platform,"real") ~ "Real-world"
)

# ============================================================
# 3 Platform colour scheme
# ============================================================

colourScale <- '
d3.scaleOrdinal()
.domain(["Simulator","Real-world"])
.range(["#4E79A7","#F28E2B"])
'

# ============================================================
# 4 Sankey Builder Function
# ============================================================

build_sankey <- function(df, stage_cols, stage_labels){
  
  # ---- Build links ----
  
  make_links <- function(df, from_col, to_col){
    
    df %>%
      count(.data[[from_col]], .data[[to_col]], platform) %>%
      
      mutate(
        
        source = paste(from_col,.data[[from_col]],sep="__"),
        target = paste(to_col,.data[[to_col]],sep="__"),
        
        source_label = as.character(.data[[from_col]]),
        target_label = as.character(.data[[to_col]])
        
      ) %>%
      
      select(source,target,value=n,platform)
    
  }
  
  links <- map2_dfr(
    stage_cols[-length(stage_cols)],
    stage_cols[-1],
    ~make_links(df,.x,.y)
  )
  
  # ---- Create nodes ----
  
  nodes <- data.frame(
    name = unique(c(links$source,links$target))
  )
  
  nodes$label <- sub(".*__","",nodes$name)
  
  # ---- Match indices ----
  
  links$source <- match(links$source,nodes$name)-1
  links$target <- match(links$target,nodes$name)-1
  
  # ---- Build Sankey ----
  
  p <- sankeyNetwork(
    
    Links = links,
    Nodes = nodes,
    
    Source = "source",
    Target = "target",
    Value = "value",
    
    NodeID = "label",
    
    LinkGroup = "platform",
    
    fontSize = 14,
    nodeWidth = 12,
    nodePadding = 8,
    fontFamily = "sans",
    
    sinksRight = FALSE,
    colourScale = colourScale,
    iterations = 0,
    
    width = 1200,
    height = 500
    
  )
  
  # ============================================================
  # Add stage labels + grey nodes
  # ============================================================
  
  p <- onRender(
    p,
    sprintf(
      "
function(el,x){

var svg = d3.select(el).select('svg');

var stageNames = %s;

var height = +svg.attr('height') || 500;
var y = height - 5;
var lineGap = 15;

var nodes = svg.selectAll('g.node').data();

var xs = Array.from(new Set(nodes.map(function(d){ return d.x; })))
  .sort(function(a,b){ return a-b; });

xs.forEach(function(xpos,i){

  var lines = stageNames[i] || ['',''];

  var txt = svg.append('text')
  .attr('x', xpos + 5)
  .attr('y', y)
  .style('font-size','14px')
  .style('fill','#000')
  .style('font-family','sans-serif')
  .style('font-weight','bold');

txt.append('tspan')
  .attr('x', xpos + 5)
  .text(lines[0]);

txt.append('tspan')
  .attr('x', xpos + 5)
  .attr('dy', lineGap)
  .text(lines[1]);

svg.selectAll('g.node text')
  .style('font-family', 'sans-serif')
  .style('font-weight', 'normal');

});

// --------------------------------------
// Draw transparent background boxes
// --------------------------------------

function drawLabelBoxes(){

  svg.selectAll('g.node').each(function(){

    var g = d3.select(this);
    var text = g.select('text');

    if(text.empty()) return;

    g.selectAll('rect.label-bg').remove();

    var bb = text.node().getBBox();

    var leftPad = 4;
    var rightPad = 18;   // extra space to the right
    var topPad = 2;
    var bottomPad = 2;

    g.insert('rect', 'text')
      .attr('class', 'label-bg')
      .attr('x', bb.x - leftPad)
      .attr('y', bb.y - topPad)
      .attr('width', bb.width + leftPad + rightPad)
      .attr('height', bb.height + topPad + bottomPad)
      .attr('rx', 4)
      .attr('ry', 4)
      .style('fill', 'white')
      .style('opacity', 0.7)
      .style('stroke', '#999')
      .style('stroke-width', 0.4);

  });

}

drawLabelBoxes();

// --------------------------------------
// Make all actual node rects grey first
// --------------------------------------

svg.selectAll('g.node').each(function(){
  var g = d3.select(this);

  // first rect = actual sankey node rect
  var nodeRect = d3.select(g.selectAll('rect').nodes()[0]);

  nodeRect
    .style('fill','#D9D9D9')
    .style('stroke','#888');
});

// --------------------------------------
// Color only the first column (Platform)
// --------------------------------------

var minX = d3.min(nodes, function(d){ return d.x; });

svg.selectAll('g.node').each(function(d){

  var g = d3.select(this);
  var label = (d.name || d.label || '').trim();

  // first rect = actual sankey node rect
  var nodeRect = d3.select(g.selectAll('rect').nodes()[0]);

  if(d.x === minX){

    if(label === 'Simulator'){
      nodeRect
        .style('fill','#4E79A7')
        .style('stroke','#4E79A7');
    }

    if(label === 'Real-world'){
      nodeRect
        .style('fill','#F28E2B')
        .style('stroke','#F28E2B');
    }
  }

});
svg.selectAll('g.node text')
  .style('font-family', 'sans-serif');

}
",
jsonlite::toJSON(stage_labels, auto_unbox = TRUE)
    )
  )
  return(p)
  
}

# ============================================================
# 5 Experimental Setup Sankey
# ============================================================

exp_cols <- c(
  "platform",
  "driving_mode",
  "cabin_structure",
  "cabin_visibility",
  "motion_dof",
  "display"
)

exp_labels <- list(
  c("Platform",""),
  c("Driving","Mode"),
  c("Cabin","Structure"),
  c("Cabin","Visibility"),
  c("Motion","DOF"),
  c("Display","Technology")
)

exp_sankey <- build_sankey(data,exp_cols,exp_labels)
exp_sankey

# ============================================================
# 6 Study Focus Sankey
# ============================================================

focus_cols <- c(
  "focus_group",
  "participant_seating_position",
  "scenario",
  "controlled_sensory_focus",
  "driver_presence_visibility"
)

focus_labels <- list(
  c("Focus","Group"),
  c("Seating","Position"),
  c("Scenario","Type"),
  c("Sensory","Focus"),
  c("Driver","Presence")
)

focus_sankey <- build_sankey(data,focus_cols,focus_labels)
focus_sankey


# ============================================================
# 7 Evaluation Measures Sankey
# ============================================================

eval_cols <- c(
  "comfort_factor",
  "measurements",
  "measurement_domain",
  "focus_condition"
)

eval_labels <- list(
  c("Comfort","Factor"),
  c("Measurement","Type"),
  c("Measurement","Domain"),
  c("Focus","Condition")
)

eval_sankey <- build_sankey(data,eval_cols,eval_labels)
eval_sankey


# ============================================================
# 8 Full Design Space Sankey
# ============================================================

full_cols <- c(
  "platform",
  "driving_mode",
  "cabin_structure",
  "scenario",
  "measurements",
  "focus_condition"
)

full_labels <- list(
  c("Platform",""),
  c("Driving","Mode"),
  c("Cabin","Structure"),
  c("Scenario","Type"),
  c("Measurement","Type"),
  c("Focus","Condition")
)

full_sankey <- build_sankey(data,full_cols,full_labels)
full_sankey

saveWidget(exp_sankey, "exp_sankey.html", selfcontained = TRUE)
saveWidget(focus_sankey, "focus_sankey.html", selfcontained = TRUE)
saveWidget(eval_sankey, "eval_sankey.html", selfcontained = TRUE)
saveWidget(full_sankey, "full_sankey.html", selfcontained = TRUE)