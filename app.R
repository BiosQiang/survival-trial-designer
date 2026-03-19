library(shiny)
library(gsDesign)
library(writexl)
library(plotly)
library(scales)

# ============================================================
# 全局色板
# ============================================================
clr <- list(
  bg      = "#0f1117", surface = "#181c27", border  = "#2a2f3e",
  text    = "#d4dbe8", muted   = "#8a96ab",
  accent  = "#00c8a0", accent2 = "#4e9eff",
  warn    = "#ffb347", purple  = "#b48eff", danger  = "#ff5f5f"
)

# ── 统一 Plotly 基础布局 ──────────────────────────────────────
apply_plotly_theme <- function(p, title="", subtitle="",
                               xlab="", ylab="", ylab2=NULL) {
  ann <- if (nchar(subtitle) > 0)
    paste0("<b>", title, "</b><br><sup style='color:#8a96ab'>", subtitle, "</sup>")
  else
    paste0("<b>", title, "</b>")
  
  p <- p |> layout(
    title = list(text=ann, font=list(color="#d4dbe8", size=15,
                                     family="'IBM Plex Mono',monospace"),
                 x=0.02, xanchor="left", y=0.97, yanchor="top"),
    paper_bgcolor = "#0f1117",
    plot_bgcolor  = "#181c27",
    font = list(family="'IBM Plex Mono',monospace", color="#d4dbe8", size=12),
    xaxis = list(
      title     = list(text=xlab, font=list(color="#d4dbe8", size=13)),
      gridcolor = "#2a2f3e", gridwidth=1,
      linecolor = "#2a2f3e", zerolinecolor="#2a2f3e",
      tickfont  = list(color="#8a96ab", size=11),
      showgrid  = TRUE
    ),
    yaxis = list(
      title     = list(text=ylab, font=list(color="#d4dbe8", size=13)),
      gridcolor = "#2a2f3e", gridwidth=1,
      linecolor = "#2a2f3e", zerolinecolor="#2a2f3e",
      tickfont  = list(color="#8a96ab", size=11),
      showgrid  = TRUE
    ),
    legend = list(
      bgcolor     = "#181c27",
      bordercolor = "#2a2f3e",
      borderwidth = 1,
      font        = list(color="#d4dbe8", size=12,
                         family="'IBM Plex Mono',monospace"),
      orientation = "h",
      y = -0.18, x = 0, xanchor = "left"
    ),
    hoverlabel = list(
      bgcolor     = "#1e2538",
      bordercolor = "#4e9eff",
      font        = list(color="#d4dbe8", size=12,
                         family="'IBM Plex Mono',monospace"),
      align       = "left"
    ),
    margin = list(l=64, r=48, t=90, b=80)
  )
  
  if (!is.null(ylab2)) {
    p <- p |> layout(
      yaxis2 = list(
        title    = list(text=ylab2$title,
                        font=list(color=ylab2$color, size=13)),
        tickfont = list(color=ylab2$color, size=11),
        overlaying="y", side="right",
        gridcolor="#2a2f3e", showgrid=FALSE,
        linecolor="#2a2f3e"
      )
    )
  }
  
  p |> config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("select2d","lasso2d","autoScale2d"),
    displaylogo = FALSE,
    toImageButtonOptions = list(format="png", scale=2.5,
                                filename="survival_trial_plot")
  )
}

# 占位图（未计算时）
no_calc_plotly <- function() {
  plot_ly() |>
    add_annotations(x=0.5, y=0.55, text="◈",
                    font=list(size=60, color="#2a2f3e"),
                    showarrow=FALSE, xref="paper", yref="paper") |>
    add_annotations(x=0.5, y=0.38,
                    text="请先在「⬡ 计算」页运行计算",
                    font=list(size=14, color="#5a6478",
                              family="'IBM Plex Mono',monospace"),
                    showarrow=FALSE, xref="paper", yref="paper") |>
    layout(paper_bgcolor="#0f1117", plot_bgcolor="#0f1117",
           xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
           margin=list(l=0,r=0,t=0,b=0)) |>
    config(displayModeBar=FALSE)
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@300;400;600&display=swap');
      :root{
        --bg:#0f1117; --surface:#181c27; --border:#2a2f3e;
        --accent:#00c8a0; --accent2:#4e9eff; --warn:#ffb347;
        --danger:#ff5f5f; --text:#d4dbe8; --muted:#8a96ab; --radius:6px;
      }
      *{box-sizing:border-box;margin:0;padding:0;}
      body{background:var(--bg);color:var(--text);font-family:'IBM Plex Sans',sans-serif;font-size:14px;min-height:100vh;}
      .app-header{background:var(--surface);border-bottom:1px solid var(--border);padding:14px 32px;display:flex;align-items:center;gap:16px;}
      .app-header h1{font-family:'IBM Plex Mono',monospace;font-size:16px;font-weight:600;color:var(--accent);letter-spacing:.04em;}
      .app-header span{font-size:11px;color:var(--muted);font-family:'IBM Plex Mono',monospace;flex:1;}
      .nav-tabs{border-bottom:1px solid var(--border)!important;padding:0 32px;background:var(--surface);}
      .nav-tabs>li>a{font-family:'IBM Plex Mono',monospace!important;font-size:11px!important;font-weight:600!important;letter-spacing:.08em!important;text-transform:uppercase!important;color:var(--muted)!important;background:transparent!important;border:none!important;border-bottom:2px solid transparent!important;border-radius:0!important;padding:10px 16px!important;margin-bottom:-1px!important;transition:color .15s,border-color .15s!important;}
      .nav-tabs>li>a:hover{color:var(--text)!important;border-bottom-color:var(--border)!important;}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover{color:var(--accent)!important;border-bottom-color:var(--accent)!important;}
      .tab-content{background:var(--bg);}
      .main-layout{display:flex;min-height:calc(100vh - 96px);}
      .sidebar{width:296px;min-width:296px;background:var(--surface);border-right:1px solid var(--border);padding:20px 18px;overflow-y:auto;}
      .content{flex:1;padding:24px 28px;overflow-y:auto;}
      .param-group{margin-bottom:20px;}
      .param-group-title{font-family:'IBM Plex Mono',monospace;font-size:9px;font-weight:600;letter-spacing:.12em;text-transform:uppercase;color:var(--muted);border-bottom:1px solid var(--border);padding-bottom:5px;margin-bottom:10px;}
      .control-label,label{font-size:11px!important;color:var(--muted)!important;font-family:'IBM Plex Mono',monospace!important;margin-bottom:3px!important;display:block!important;}
      input[type=number],input[type=text],select,.form-control{background:var(--bg)!important;border:1px solid var(--border)!important;border-radius:var(--radius)!important;color:var(--text)!important;font-family:'IBM Plex Mono',monospace!important;font-size:12px!important;padding:5px 9px!important;width:100%!important;transition:border-color .15s;}
      input[type=number]:focus,.form-control:focus{border-color:var(--accent)!important;outline:none!important;box-shadow:0 0 0 2px rgba(0,200,160,.12)!important;}
      .shiny-input-container{margin-bottom:8px!important;}
      #run_btn{width:100%;background:var(--accent);color:#0f1117;border:none;border-radius:var(--radius);font-family:'IBM Plex Mono',monospace;font-size:12px;font-weight:600;letter-spacing:.06em;padding:9px;cursor:pointer;margin-top:2px;transition:opacity .15s,transform .1s;}
      #run_btn:hover{opacity:.88;transform:translateY(-1px);}
      #run_btn:active{transform:translateY(0);}
      .btn-sub{width:100%;border-radius:var(--radius);font-family:'IBM Plex Mono',monospace;font-size:11px;font-weight:600;letter-spacing:.05em;padding:7px 8px;cursor:pointer;transition:background .15s;display:block;margin-top:8px;text-align:center;border:none;}
      #save_btn{background:rgba(0,200,160,.12);color:#00c8a0;border:1px solid rgba(0,200,160,.3)!important;}
      #save_btn:hover{background:rgba(0,200,160,.22)!important;}
      /* ── Result Cards ── */
      .results-grid{display:grid;grid-template-columns:1fr 1fr;gap:16px;margin-bottom:18px;}
      .result-card{background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);padding:16px 18px;position:relative;overflow:hidden;}
      .result-card::before{content:'';position:absolute;top:0;left:0;right:0;height:3px;}
      .result-card.green::before{background:var(--accent);}
      .result-card.blue::before{background:var(--accent2);}
      .result-card.orange::before{background:var(--warn);}
      .result-card.purple::before{background:#b48eff;}
      .card-tag{font-family:'IBM Plex Mono',monospace;font-size:9px;font-weight:600;letter-spacing:.14em;text-transform:uppercase;color:var(--muted);margin-bottom:12px;}
      /* ── Stat rows: 文字可读性修复 ── */
      .stat-row{display:flex;justify-content:space-between;align-items:baseline;padding:7px 0;border-bottom:1px solid rgba(255,255,255,.07);}
      .stat-row:last-child{border-bottom:none;}
      .stat-label{font-size:13px;color:#c2cbd8;font-family:'IBM Plex Sans',sans-serif;font-weight:400;letter-spacing:.01em;}
      .stat-value{font-family:'IBM Plex Mono',monospace;font-size:14px;font-weight:600;color:#e8edf5;}
      .stat-value.highlight-green {color:var(--accent);}
      .stat-value.highlight-blue  {color:var(--accent2);}
      .stat-value.highlight-orange{color:var(--warn);}
      .stat-value.highlight-purple{color:#b48eff;}
      .big-stat{text-align:center;padding:8px 0 12px;}
      .big-number{font-family:'IBM Plex Mono',monospace;font-size:38px;font-weight:600;line-height:1;letter-spacing:-.02em;}
      .big-label{font-size:11px;color:var(--muted);margin-top:3px;}
      .big-divider{display:flex;gap:1px;margin:10px 0 3px;}
      .big-divider span{flex:1;height:3px;border-radius:2px;background:var(--border);}
      .big-divider span.filled{background:var(--accent);}
      .error-box{background:rgba(255,80,80,.08);border:1px solid rgba(255,80,80,.3);border-radius:var(--radius);padding:12px 16px;color:#ff8080;font-family:'IBM Plex Mono',monospace;font-size:12px;margin-bottom:16px;}
      .placeholder{display:flex;flex-direction:column;align-items:center;justify-content:center;height:280px;color:var(--muted);font-family:'IBM Plex Mono',monospace;font-size:11px;letter-spacing:.06em;gap:8px;}
      .placeholder-icon{font-size:28px;opacity:.3;}
      /* ── Compare tab ── */
      .compare-toolbar{display:flex;align-items:center;gap:10px;margin-bottom:16px;padding-bottom:14px;border-bottom:1px solid var(--border);flex-wrap:wrap;}
      .compare-toolbar-title{font-family:'IBM Plex Mono',monospace;font-size:10px;font-weight:600;letter-spacing:.1em;text-transform:uppercase;color:var(--muted);flex:1;min-width:120px;}
      .record-count{font-family:'IBM Plex Mono',monospace;font-size:11px;color:var(--muted);background:var(--surface);border:1px solid var(--border);border-radius:12px;padding:2px 10px;}
      #delete_btn{background:rgba(255,95,95,.1);color:#ff5f5f;border:1px solid rgba(255,95,95,.3)!important;border-radius:var(--radius);font-family:'IBM Plex Mono',monospace;font-size:11px;font-weight:600;padding:6px 12px;cursor:pointer;transition:background .15s;}
      #delete_btn:hover{background:rgba(255,95,95,.2)!important;}
      #export_btn{background:rgba(78,158,255,.12);color:var(--accent2);border:1px solid rgba(78,158,255,.3)!important;border-radius:var(--radius);font-family:'IBM Plex Mono',monospace;font-size:11px;font-weight:600;padding:6px 12px;cursor:pointer;transition:background .15s;}
      #export_btn:hover{background:rgba(78,158,255,.22)!important;}
      .compare-table-wrap{overflow-x:auto;border:1px solid var(--border);border-radius:var(--radius);}
      .compare-table{width:100%;border-collapse:collapse;font-family:'IBM Plex Mono',monospace;font-size:12px;min-width:960px;}
      .compare-table th{background:var(--surface);color:var(--muted);font-size:9px;font-weight:600;letter-spacing:.08em;text-transform:uppercase;padding:9px 12px;border-bottom:1px solid var(--border);white-space:nowrap;text-align:left;}
      .compare-table th:first-child{width:36px;text-align:center;}
      .compare-table td{padding:8px 12px;border-bottom:1px solid rgba(255,255,255,.04);color:var(--text);white-space:nowrap;}
      .compare-table td:first-child{text-align:center;}
      .compare-table tr:last-child td{border-bottom:none;}
      .compare-table tr:hover td{background:rgba(255,255,255,.025);}
      .compare-table tr.selected-row td{background:rgba(0,200,160,.06);}
      .compare-table input[type=checkbox]{width:14px!important;height:14px!important;accent-color:var(--accent);cursor:pointer;padding:0!important;}
      .compare-table td.num{color:var(--accent2);}
      .empty-compare{padding:60px 20px;text-align:center;color:var(--muted);font-family:'IBM Plex Mono',monospace;font-size:11px;letter-spacing:.04em;}
      /* ── Plot tab ── */
      .plot-area{padding:24px 28px;}
      .plot-card{background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);overflow:hidden;}
      .plot-subtabs .nav-tabs{padding:0!important;background:transparent!important;border-bottom:1px solid var(--border)!important;margin:0 0 20px 0;}
      .plot-subtabs .nav-tabs>li>a{padding:8px 14px!important;font-size:10px!important;}
      .plotly.html-widget,.js-plotly-plot{background:#0f1117!important;}
      .container-fluid{padding:0!important;}.row{margin:0!important;}
    ")),
    tags$script(HTML("
      $(document).on('click','#chk_all',function(){
        var c=this.checked;
        $('.row-chk').prop('checked',c);
        $('.compare-table tr[data-row]').toggleClass('selected-row',c);
      });
      $(document).on('change','.row-chk',function(){
        $(this).closest('tr').toggleClass('selected-row',this.checked);
        var t=$('.row-chk').length, n=$('.row-chk:checked').length;
        $('#chk_all').prop('indeterminate',n>0&&n<t).prop('checked',n===t&&t>0);
      });
      Shiny.addCustomMessageHandler('get_checked_rows',function(msg){
        var c=[];
        $('.row-chk:checked').each(function(){c.push(parseInt($(this).attr('data-row')));});
        Shiny.setInputValue('checked_rows',c,{priority:'event'});
      });
    "))
  ),
  
  div(class="app-header",
      h1("SURVIVAL TRIAL DESIGNER"),
      span("// gsDesign · 生存分析样本量计算工具")
  ),
  
  tabsetPanel(id="main_tabs",
              
              # ══ TAB 1 · 计算 ════════════════════════════════════════
              tabPanel("⬡  计算",
                       div(class="main-layout",
                           div(class="sidebar",
                               div(class="param-group",
                                   div(class="param-group-title","生存参数"),
                                   numericInput("median_c","对照组中位生存时间（月）",value=7,   min=0.1,step=0.5),
                                   numericInput("median_t","试验组中位生存时间（月）",value=11.8,min=0.1,step=0.5)
                               ),
                               div(class="param-group",
                                   div(class="param-group-title","研究设计"),
                                   selectInput("sided","检验方向",choices=c("双侧"=2,"单侧"=1),selected=2),
                                   numericInput("alpha","I类错误 α",              value=0.05,min=0.001,max=0.5, step=0.005),
                                   numericInput("beta", "II类错误 β（把握度=1-β）",value=0.2, min=0.01, max=0.99,step=0.05),
                                   numericInput("ratio","分配比例（试验:对照）",   value=1,   min=0.1,           step=0.1)
                               ),
                               div(class="param-group",
                                   div(class="param-group-title","入组与随访"),
                                   numericInput("Tr",           "入组时间 Tr（月）",  value=5,   min=1,  step=1),
                                   numericInput("Ts",           "总研究时间 Ts（月）",value=16,  min=2,  step=1),
                                   numericInput("eta_year_perc","年脱落率",           value=0.05,min=0,max=0.99,step=0.01)
                               ),
                               div(class="param-group",
                                   div(class="param-group-title","功能3 · 期中预期事件数"),
                                   numericInput("tIA_f3","指定日历时间（月，可留空）",value=NA,min=0.1,step=1)
                               ),
                               div(class="param-group",
                                   div(class="param-group-title","功能4 · 所需日历时间"),
                                   numericInput("nevent_f4","目标事件数（可留空）",value=NA,min=1,step=1)
                               ),
                               actionButton("run_btn","▶  运行计算"),
                               uiOutput("action_btns_ui")
                           ),
                           div(class="content", uiOutput("results_ui"))
                       )
              ),
              
              # ══ TAB 2 · 可视化 ══════════════════════════════════════
              tabPanel("◈  可视化",
                       div(class="plot-area",
                           div(class="plot-subtabs",
                               tabsetPanel(id="plot_tabs",
                                           tabPanel("事件累积曲线", br(),
                                                    div(class="plot-card", plotlyOutput("plot_events", height="480px"))),
                                           tabPanel("把握度曲线", br(),
                                                    div(class="plot-card", plotlyOutput("plot_power",  height="480px"))),
                                           tabPanel("敏感性分析", br(),
                                                    div(class="plot-card", plotlyOutput("plot_sens",   height="480px"))),
                                           tabPanel("入组 & 事件进度甘特图", br(),
                                                    div(class="plot-card", plotlyOutput("plot_gantt",  height="500px")))
                               )
                           )
                       )
              ),
              
              # ══ TAB 3 · 已保存结果 ══════════════════════════════════
              tabPanel("📋  已保存结果",
                       div(style="padding:24px 28px;",
                           div(class="compare-toolbar",
                               div(class="compare-toolbar-title","历史计算记录"),
                               uiOutput("record_count_ui"),
                               actionButton("delete_btn",  "✕  删除选中"),
                               downloadButton("export_btn","↓  导出 Excel")
                           ),
                           uiOutput("compare_table_ui")
                       )
              )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  saved_results <- reactiveVal(data.frame())
  last_result   <- reactiveVal(NULL)
  
  # ── 核心计算 ──────────────────────────────────────────────
  do_calc <- function() {
    sided       <- as.integer(input$sided)
    alpha       <- input$alpha;  beta  <- input$beta
    ratio       <- input$ratio;  Tr    <- input$Tr;  Ts <- input$Ts
    minfup      <- Ts - Tr
    eta_monthly <- log(1/(1-input$eta_year_perc))/12
    lambda_ctrl <- log(2)/input$median_c
    lambda_trt  <- log(2)/input$median_t
    hr          <- input$median_c/input$median_t
    ratio_f2    <- 1/(1+ratio)
    if (Ts<=Tr) stop("总研究时间 Ts 必须大于入组时间 Tr")
    if (input$median_c<=0||input$median_t<=0) stop("中位生存时间必须大于0")
    # 统一用 nSurv() 作为唯一计算入口
    # R=Tr + minfup 同时传入，三者满足 T = R + minfup，gamma 由函数内部反算
    x_surv <- nSurv(lambdaC=lambda_ctrl, hr=hr, hr0=1,
                    alpha=alpha, beta=beta, sided=sided,
                    eta=eta_monthly, T=Ts, R=Tr, minfup=minfup, ratio=ratio)
    # 打印 x_surv 关键字段，用于调试
    message("=== x_surv fields ===")
    message("n     = ", paste(x_surv$n,     collapse=", "))
    message("eD    = ", paste(x_surv$eDC+x_surv$eDE, collapse=", "))
    message("eDC   = ", paste(x_surv$eDC,   collapse=", "))
    message("eDE   = ", paste(x_surv$eDE,   collapse=", "))
    message("gamma = ", paste(x_surv$gamma, collapse=", "))
    message("names = ", paste(names(x_surv), collapse=", "))
    # 总样本量与事件数（全部强制转为标量）
    n_total     <- as.integer(ceiling(as.numeric(x_surv$n)))
    n_events_f1 <- as.integer(ceiling(as.numeric(x_surv$eDC + x_surv$eDE)))
    gamma       <- n_total / Tr
    # 各组样本量（按 ratio 拆分）
    n_trt    <- as.integer(ceiling(n_total * ratio / (1 + ratio)))
    n_ctrl   <- as.integer(n_total - n_trt)
    # 各组期望事件数：eDC=对照组，eDE=试验组（nSurv 字段名）
    ev_ctrl  <- as.integer(ceiling(as.numeric(x_surv$eDC)))
    ev_trt   <- as.integer(ceiling(as.numeric(x_surv$eDE)))
    # 功能2：用功能1事件数联动
    HR_detectable <- exp(-qnorm(1-alpha/sided)*
                           sqrt((1/(ratio_f2*(1-ratio_f2)))/n_events_f1))
    min_surv_test <- input$median_c/HR_detectable
    # 功能3：可选，空则跳过
    tIA_val <- input$tIA_f3
    if (is.na(tIA_val) || is.null(tIA_val) || length(tIA_val)==0) {
      events_at_tIA <- NULL
    } else {
      events_at_tIA <- nEventsIA(x=x_surv, tIA=tIA_val)
    }
    # 功能4：可选，空则跳过
    nev4_val <- input$nevent_f4
    if (is.na(nev4_val) || is.null(nev4_val) || length(nev4_val)==0) {
      T_required <- NULL
    } else {
      T_required <- uniroot(f=function(T) nev4_val-nEventsIA(x=x_surv,tIA=T),
                            interval=c(0.5,500))$root
    }
    list(sided=sided, alpha=alpha, beta=beta, ratio=ratio,
         Tr=Tr, Ts=Ts, minfup=minfup,
         eta_year_perc=input$eta_year_perc,
         eta_monthly=eta_monthly, gamma=gamma,
         median_c=input$median_c, median_t=input$median_t,
         hr=hr, ratio_f2=ratio_f2,
         n_events=n_events_f1, n_total=n_total,
         n_trt=n_trt, n_ctrl=n_ctrl,
         ev_trt=ev_trt, ev_ctrl=ev_ctrl,
         HR_detectable=HR_detectable, min_surv_test=min_surv_test,
         tIA_f3=if(is.na(tIA_val)||is.null(tIA_val)) NULL else tIA_val,
         events_at_tIA=events_at_tIA,
         nevent_f4=if(is.na(nev4_val)||is.null(nev4_val)) NULL else nev4_val,
         T_required=T_required,
         x_surv=x_surv,
         r_version=paste0(R.version$major,".",R.version$minor),
         gsdesign_version=as.character(packageVersion("gsDesign")))
  }
  
  calc_result <- eventReactive(input$run_btn,{
    tryCatch(do_calc(),error=function(e) list(error=conditionMessage(e)))
  })
  observe({
    req(input$run_btn>0); res <- calc_result()
    if (is.null(res$error)) last_result(res)
  })
  
  # ── 次级按钮 ──────────────────────────────────────────────
  output$action_btns_ui <- renderUI({
    req(!is.null(last_result()))
    tagList(
      actionButton("save_btn","＋  保存本次结果",class="btn-sub"),
      downloadButton("report_dl","📄  生成样本量报告",
                     style="width:100%;background:rgba(78,158,255,.12);color:#4e9eff;
               border:1px solid rgba(78,158,255,.3);border-radius:6px;
               font-family:'IBM Plex Mono',monospace;font-size:11px;
               font-weight:600;padding:7px 8px;margin-top:8px;text-align:center;")
    )
  })
  
  # ── Rmd 报告 ──────────────────────────────────────────────
  output$report_dl <- downloadHandler(
    filename=function() paste0("sample_size_report_",
                               format(Sys.time(),"%Y%m%d_%H%M%S"),".Rmd"),
    content=function(file){
      res <- last_result(); req(!is.null(res))
      sided_label_cn <- if(res$sided==2)"双侧"else"单侧"
      sided_label_en <- if(res$sided==2)"two-sided"else"one-sided"
      power_pct      <- round((1-res$beta)*100)
      alpha_en       <- sprintf("%.3f", res$alpha)
      hr_fmt         <- sprintf("%.4f", res$hr)
      eta_pct        <- round(res$eta_year_perc*100)
      rmd <- paste0(
        '---
title: "样本量估算报告 / Sample Size Estimation Report"
date: "',format(Sys.time(),"%Y-%m-%d"),'"
output:
  word_document:
    toc: false
  html_document:
    toc: false
    theme: flatly
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, message=FALSE, warning=FALSE)
```

---

# 一、中文描述

## 1.1 样本量估算

本研究的主要终点为时间-事件（time-to-event）终点。在等比例风险（proportional hazards）假设下，
生存时间服从指数分布，采用基于对数秩检验（log-rank test）的事件驱动设计。
样本量估算使用 R 软件（版本 ',res$r_version,'）中 gsDesign 包（版本 ',res$gsdesign_version,'）
的 `nSurv()` 函数实现，该函数基于 Lachin & Foulkes (1986) 提出的方法，
对均匀入组（uniform accrual）及竞争性删失（competing dropout）条件下的事件驱动试验进行精确功效计算。

根据历史数据，对照组中位生存时间约为 ',res$median_c,' 个月
（对应风险率 λ~C~ = ',sprintf("%.4f",log(2)/res$median_c),' /月）。
假设试验组中位生存时间为 ',res$median_t,' 个月
（对应风险率 λ~E~ = ',sprintf("%.4f",log(2)/res$median_t),' /月），
相应风险比（hazard ratio，HR）为 ',hr_fmt,'。

检验采用',sided_label_cn,'对数秩检验，I 类错误率（α）= ',res$alpha,'，
目标统计检验效能（1−β）= ',power_pct,'%。
年脱落率（dropout rate）设定为 ',eta_pct,'%（月脱落风险率 η = ',sprintf("%.4f",res$eta_monthly),' /月）。
入组期（accrual period）',res$Tr,' 个月，末例入组后最短随访期（minimum follow-up）',res$minfup,' 个月，
总研究时间 ',res$Ts,' 个月，试验组与对照组分配比例为 ',res$ratio,' : 1。

基于上述假设，研究达到预设检验效能所需的**最少事件数为 ',res$n_events,' 例**
（试验组 ',res$ev_trt,' 例，对照组 ',res$ev_ctrl,' 例）。
相应地，**计划入组总样本量为 ',res$n_total,' 例**
（试验组 ',res$n_trt,' 例，对照组 ',res$n_ctrl,' 例）。

> **参考文献：** Lachin JM, Foulkes MA. Evaluation of sample size and power for analyses of survival with allowance for nonuniform patient entry, losses to follow-up, noncompliance, and stratification. *Biometrics*. 1986;42(3):507–519.

---

# 二、English Description

## 2.1 Sample Size Estimation

The primary endpoint of this study is a time-to-event outcome. Under the assumption of proportional hazards,
survival times are modeled using an exponential distribution, and the study employs an event-driven design
based on the log-rank test. Sample size was calculated using the `nSurv()` function from the gsDesign package
(version ',res$gsdesign_version,') in R (version ',res$r_version,'), which implements the method of
Lachin & Foulkes (1986) to provide exact power calculations under uniform accrual and competing dropout.

Based on historical data, the median survival time in the control arm is approximately ',res$median_c,' months
(corresponding hazard rate λ~C~ = ',sprintf("%.4f",log(2)/res$median_c),' per month).
The treatment arm is assumed to achieve a median survival time of ',res$median_t,' months
(hazard rate λ~E~ = ',sprintf("%.4f",log(2)/res$median_t),' per month),
yielding a hazard ratio (HR) of ',hr_fmt,'.

A ',sided_label_en,' log-rank test will be used with a type I error rate of α = ',alpha_en,'
and a target power of ',power_pct,'%. The annual dropout rate is assumed to be ',eta_pct,'%
(monthly dropout hazard η = ',sprintf("%.4f",res$eta_monthly),' per month).
The accrual period is ',res$Tr,' months, with a minimum follow-up of ',res$minfup,' months
after the last patient enrolled, for a total study duration of ',res$Ts,' months.
Patients will be randomized in a ',res$ratio,' : 1 ratio (experimental : control).

Based on these assumptions, a minimum of **',res$n_events,' events** are required to achieve
the pre-specified power (',res$ev_trt,' in the experimental arm and ',res$ev_ctrl,' in the control arm).
This corresponds to a planned total sample size of **',res$n_total,' patients**
(',res$n_trt,' in the experimental arm and ',res$n_ctrl,' in the control arm).

> **Reference:** Lachin JM, Foulkes MA. Evaluation of sample size and power for analyses of survival with allowance for nonuniform patient entry, losses to follow-up, noncompliance, and stratification. *Biometrics*. 1986;42(3):507–519.

---

# 三、计算代码 / Reproducible Code

```{r}
library(gsDesign)

# 参数定义 / Parameter definition
median_c     <- ',res$median_c,'   # 对照组中位生存时间（月）/ Control arm median OS (months)
median_t     <- ',res$median_t,'   # 试验组中位生存时间（月）/ Experimental arm median OS (months)
sided        <- ',res$sided,'      # 检验方向 / Test sides
alpha        <- ',res$alpha,'      # I 类错误率 / Type I error
beta         <- ',res$beta,'       # II 类错误率 / Type II error
ratio        <- ',res$ratio,'      # 随机化比例（试验:对照）/ Randomization ratio (E:C)
Tr           <- ',res$Tr,'         # 入组期（月）/ Accrual period (months)
Ts           <- ',res$Ts,'         # 总研究时间（月）/ Total study duration (months)
eta_year     <- ',res$eta_year_perc,'  # 年脱落率 / Annual dropout rate

# 参数推导 / Derived parameters
lambda_ctrl  <- log(2) / median_c
lambda_trt   <- log(2) / median_t
hr           <- lambda_trt / lambda_ctrl
eta_monthly  <- log(1 / (1 - eta_year)) / 12
minfup       <- Ts - Tr

# 样本量计算（nSurv）/ Sample size calculation
x <- nSurv(lambdaC  = lambda_ctrl,
           hr       = hr,
           hr0      = 1,
           alpha    = alpha,
           beta     = beta,
           sided    = sided,
           eta      = eta_monthly,
           T        = Ts,
           R        = Tr,
           minfup   = minfup,
           ratio    = ratio)

cat("所需事件数（总）Events required (total) :", ceiling(x$eDC + x$eDE), "\\n")
cat("所需事件数（试验组）Events (experimental) :", ceiling(x$eDE), "\\n")
cat("所需事件数（对照组）Events (control)      :", ceiling(x$eDC), "\\n")
cat("总样本量 Total sample size              :", ceiling(x$n),  "\\n")
```

---

*本报告由 Survival Trial Designer 自动生成 / Auto-generated by Survival Trial Designer*  
*生成时间 / Generated: ',format(Sys.time(),"%Y-%m-%d %H:%M:%S"),'*
')
      writeLines(rmd, file)
    })
  
  # ── 保存 ──────────────────────────────────────────────────
  observeEvent(input$save_btn,{
    res <- last_result(); req(!is.null(res))
    new_row <- data.frame(
      记录编号=nrow(saved_results())+1,
      保存时间=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
      对照组中位生存_月=res$median_c, 试验组中位生存_月=res$median_t,
      HR_对照除以试验=round(res$hr,4), 显著性水平_alpha=res$alpha,
      检验方向_sided=res$sided, 目标把握度=paste0(round((1-res$beta)*100),"%"),
      所需事件数_总=res$n_events,
      所需事件数_试验组=res$ev_trt, 所需事件数_对照组=res$ev_ctrl,
      所需总样本量=res$n_total,
      样本量_试验组=res$n_trt, 样本量_对照组=res$n_ctrl,
      计划入组时间_Tr_月=res$Tr, 末例入组后随访_月=res$minfup,
      功能2_可检出最大HR=round(res$HR_detectable,4),
      功能2_可检出最小试验组中位生存_月=round(res$min_surv_test,2),
      stringsAsFactors=FALSE,check.names=FALSE)
    saved_results(rbind(saved_results(),new_row))
    updateTabsetPanel(session,"main_tabs",selected="📋  已保存结果")
  })
  
  output$record_count_ui <- renderUI({
    div(class="record-count",paste0(nrow(saved_results())," 条记录"))
  })
  
  output$compare_table_ui <- renderUI({
    df <- saved_results()
    if (nrow(df)==0) return(div(class="empty-compare","📭  暂无保存记录",
                                tags$br(),tags$span(style="opacity:.6;",
                                                    "在「⬡ 计算」页完成计算后点击「＋ 保存本次结果」")))
    col_names <- names(df)
    num_cols  <- c("对照组中位生存_月","试验组中位生存_月","HR_对照除以试验",
                   "所需事件数_总","所需事件数_试验组","所需事件数_对照组",
                   "所需总样本量","样本量_试验组","样本量_对照组",
                   "功能2_可检出最大HR","功能2_可检出最小试验组中位生存_月")
    header_row <- tags$tr(
      tags$th(tags$input(type="checkbox",id="chk_all",title="全选/取消")),
      lapply(col_names,tags$th))
    body_rows <- lapply(seq_len(nrow(df)),function(i){
      row_data <- df[i,]
      cells <- lapply(col_names,function(cn)
        tags$td(class=if(cn%in%num_cols)"num"else"",as.character(row_data[[cn]])))
      tags$tr(`data-row`=i,
              tags$td(tags$input(type="checkbox",class="row-chk",`data-row`=i)),cells)
    })
    div(class="compare-table-wrap",
        tags$table(class="compare-table",tags$thead(header_row),tags$tbody(body_rows)))
  })
  
  observeEvent(input$delete_btn,{session$sendCustomMessage("get_checked_rows",list())})
  observeEvent(input$checked_rows,{
    rows <- input$checked_rows; df <- saved_results()
    if (length(rows)==0||nrow(df)==0) return()
    df <- df[-rows,,drop=FALSE]
    if (nrow(df)>0) df$记录编号 <- seq_len(nrow(df))
    saved_results(df)
  })
  output$export_btn <- downloadHandler(
    filename=function() paste0("trial_design_",format(Sys.time(),"%Y%m%d_%H%M%S"),".xlsx"),
    content=function(file){
      df <- saved_results()
      writexl::write_xlsx(if(nrow(df)==0) data.frame(提示="暂无保存记录") else df,file)
    })
  
  # ============================================================
  # 图 1：事件累积曲线（Plotly 交互）
  # ============================================================
  output$plot_events <- renderPlotly({
    res <- last_result()
    if (is.null(res)) return(no_calc_plotly())
    
    x_surv  <- res$x_surv
    t_ref   <- if (!is.null(res$T_required)) res$T_required else res$Ts
    t_max   <- max(res$Ts*1.18, t_ref*1.12)
    t_seq   <- seq(0.1, t_max, length.out=300)
    ev_seq  <- sapply(t_seq, function(t)
      tryCatch(nEventsIA(x=x_surv,tIA=t), error=function(e) NA_real_))
    df      <- data.frame(t=t_seq, events=ev_seq)
    df      <- df[!is.na(df$events),]
    target  <- res$n_events
    ev_max  <- max(df$events, na.rm=TRUE)
    
    p <- plot_ly() |>
      add_trace(data=df, x=~t, y=~events,
                type="scatter", mode="none",
                fill="tozeroy", fillcolor="rgba(78,158,255,0.10)",
                showlegend=FALSE, hoverinfo="skip") |>
      add_trace(data=df, x=~t, y=~events,
                type="scatter", mode="lines",
                line=list(color="#4e9eff", width=2.5),
                name="累积事件数",
                customdata=round(df$events/target*100,1),
                hovertemplate=paste0(
                  "<b>日历时间：</b>%{x:.1f} 月<br>",
                  "<b>累积事件数：</b>%{y:.0f}<br>",
                  "<b>达成率：</b>%{customdata}%<extra></extra>")) |>
      add_segments(x=0, xend=t_max, y=target, yend=target,
                   line=list(color="#00c8a0", width=1.5, dash="dash"),
                   name=sprintf("目标事件 %d",target), hoverinfo="skip") |>
      add_segments(x=res$Tr, xend=res$Tr, y=0, yend=ev_max*1.05,
                   line=list(color="#8a96ab", width=1.2, dash="dot"),
                   name=sprintf("入组结束（%.0f 月）",res$Tr), hoverinfo="skip")
    
    # 功能3 点（可选）
    if (!is.null(res$tIA_f3) && !is.null(res$events_at_tIA)) {
      p <- p |>
        add_trace(x=res$tIA_f3, y=res$events_at_tIA,
                  type="scatter", mode="markers",
                  marker=list(color="#ffb347", size=14, symbol="circle",
                              line=list(color="#0f1117", width=2.5)),
                  name=sprintf("期中分析（%.0f 月）",res$tIA_f3),
                  hovertemplate=sprintf(
                    "<b>期中分析</b><br>时间：%.0f 月<br>预期事件：%.0f<br>占目标：%.1f%%<extra></extra>",
                    res$tIA_f3, res$events_at_tIA,
                    res$events_at_tIA/target*100)) |>
        add_annotations(
          x=res$tIA_f3, y=res$events_at_tIA+ev_max*0.08,
          text=sprintf("<b>%.0f 月 · %.0f 事件 (%.0f%%)</b>",
                       res$tIA_f3, res$events_at_tIA,
                       res$events_at_tIA/target*100),
          font=list(color="#ffb347",size=12),
          bgcolor="rgba(24,28,39,0.9)", bordercolor="#ffb347", borderwidth=1,
          showarrow=TRUE, arrowcolor="#ffb347", arrowwidth=1.5, ax=-45, ay=-20)
    }
    
    # 功能4 点（可选）
    if (!is.null(res$T_required) && !is.null(res$nevent_f4)) {
      p <- p |>
        add_trace(x=res$T_required, y=res$nevent_f4,
                  type="scatter", mode="markers",
                  marker=list(color="#00c8a0", size=14, symbol="circle",
                              line=list(color="#0f1117", width=2.5)),
                  name=sprintf("达到目标（%.1f 月）",res$T_required),
                  hovertemplate=sprintf(
                    "<b>达到目标事件</b><br>时间：%.1f 月<br>事件数：%d<extra></extra>",
                    res$T_required, res$nevent_f4)) |>
        add_annotations(
          x=res$T_required, y=res$nevent_f4+ev_max*0.08,
          text=sprintf("<b>%.1f 月 · %d 事件</b>",res$T_required,res$nevent_f4),
          font=list(color="#00c8a0",size=12),
          bgcolor="rgba(24,28,39,0.9)", bordercolor="#00c8a0", borderwidth=1,
          showarrow=TRUE, arrowcolor="#00c8a0", arrowwidth=1.5, ax=45, ay=-20)
    }
    
    apply_plotly_theme(p,
                       title    = "累积事件数 vs 日历时间",
                       subtitle = sprintf("总样本量 %d 例  ·  目标事件 %d  ·  均匀入组 γ = %.1f /月",
                                          res$n_total,target,res$gamma),
                       xlab = "日历时间（月）",
                       ylab = "累积事件数")
  })
  
  # ============================================================
  # 图 2：把握度 vs HR（Plotly 交互）
  # ============================================================
  output$plot_power <- renderPlotly({
    res <- last_result()
    if (is.null(res)) return(no_calc_plotly())
    
    hr_range  <- seq(0.28,0.98,length.out=400)
    ratio_f2  <- res$ratio_f2
    n_ev      <- res$n_events
    power_vec <- sapply(hr_range,function(h){
      z <- sqrt(n_ev*ratio_f2*(1-ratio_f2))*abs(log(h))-qnorm(1-res$alpha/res$sided)
      pnorm(z)*100
    })
    df_pw  <- data.frame(hr=hr_range,power=power_vec)
    df_lo  <- df_pw[df_pw$power<80,]
    df_hi  <- df_pw[df_pw$power>=80,]
    pw_cur <- approx(hr_range,power_vec,xout=res$hr)$y
    hr80   <- res$HR_detectable
    
    p <- plot_ly() |>
      add_trace(data=df_lo, x=~hr, y=~power, type="scatter", mode="none",
                fill="tozeroy", fillcolor="rgba(255,95,95,0.08)",
                showlegend=FALSE, hoverinfo="skip") |>
      add_trace(data=df_hi, x=~hr, y=~power, type="scatter", mode="none",
                fill="tozeroy", fillcolor="rgba(0,200,160,0.10)",
                showlegend=FALSE, hoverinfo="skip") |>
      add_segments(x=0.28, xend=0.98, y=80, yend=80,
                   line=list(color="#00c8a0",width=1.5,dash="dash"),
                   name="80% 把握度阈值", hoverinfo="skip") |>
      add_trace(data=df_pw, x=~hr, y=~power, type="scatter", mode="lines",
                line=list(color="#4e9eff",width=2.5),
                name="把握度曲线",
                hovertemplate="<b>HR：</b>%{x:.3f}<br><b>把握度：</b>%{y:.1f}%<extra></extra>") |>
      add_trace(x=res$hr, y=pw_cur, type="scatter", mode="markers",
                marker=list(color="#ffb347",size=14,symbol="circle",
                            line=list(color="#0f1117",width=2.5)),
                name=sprintf("设计 HR=%.2f",res$hr),
                hovertemplate=sprintf(
                  "<b>当前设计</b><br>HR：%.2f<br>把握度：%.1f%%<extra></extra>",
                  res$hr,pw_cur)) |>
      add_trace(x=hr80, y=80, type="scatter", mode="markers",
                marker=list(color="#ff5f5f",size=12,symbol="diamond",
                            line=list(color="#0f1117",width=2)),
                name=sprintf("最大可检 HR=%.4f",hr80),
                hovertemplate=sprintf(
                  "<b>最大可检HR</b><br>HR：%.4f<br>把握度：80%%<extra></extra>",hr80)) |>
      add_annotations(
        x=res$hr, y=max(pw_cur-10,5),
        text=sprintf("<b>设计 HR = %.2f</b><br>把握度 = %.1f%%",res$hr,pw_cur),
        font=list(color="#ffb347",size=12),
        bgcolor="rgba(24,28,39,0.9)", bordercolor="#ffb347", borderwidth=1,
        showarrow=TRUE, arrowcolor="#ffb347", arrowwidth=1.5, ax=-55, ay=30)
    
    apply_plotly_theme(p,
                       title    = "检验把握度 vs 风险比（HR）",
                       subtitle = sprintf("事件数 %d  ·  α = %.3f（%d 侧）  ·  分配比 %.3f : %.3f",
                                          n_ev,res$alpha,res$sided,ratio_f2,1-ratio_f2),
                       xlab = "风险比 HR（试验 / 对照）",
                       ylab = "把握度（1 − β）%") |>
      layout(
        xaxis=list(tickformat=".2f"),
        yaxis=list(range=c(0,105),ticksuffix="%")
      )
  })
  
  # ============================================================
  # 图 3：敏感性分析（Plotly 交互，双Y轴）
  # ============================================================
  output$plot_sens <- renderPlotly({
    res <- last_result()
    if (is.null(res)) return(no_calc_plotly())
    
    mt_range <- seq(res$median_c*1.06, res$median_c*3.2, length.out=60)
    results  <- lapply(mt_range,function(mt){
      lc <- log(2)/res$median_c; lt <- log(2)/mt
      tryCatch({
        n <- nSurvival(lambda1=lc,lambda2=lt,
                       sided=res$sided,alpha=res$alpha,beta=res$beta,
                       eta=res$eta_monthly,Ts=res$Ts,Tr=res$Tr,ratio=res$ratio)
        data.frame(median_t=mt,hr=res$median_c/mt,
                   nevents=ceiling(n$nEvents),ntotal=ceiling(n$n))
      },error=function(e) data.frame(median_t=mt,hr=NA,nevents=NA,ntotal=NA))
    })
    df_s <- do.call(rbind,results)
    df_s <- df_s[!is.na(df_s$nevents),]
    cur  <- data.frame(median_t=res$median_t,hr=res$hr,
                       nevents=res$n_events,ntotal=res$n_total)
    
    p <- plot_ly() |>
      add_trace(data=df_s, x=~median_t, y=~nevents,
                type="scatter", mode="lines",
                line=list(color="#4e9eff",width=2.5),
                name="所需事件数", yaxis="y",
                customdata=round(df_s$hr,3),
                hovertemplate=paste0(
                  "<b>试验组中位：</b>%{x:.1f} 月<br>",
                  "<b>对应 HR：</b>%{customdata:.3f}<br>",
                  "<b>所需事件数：</b>%{y}<extra></extra>")) |>
      add_trace(data=df_s, x=~median_t, y=~ntotal,
                type="scatter", mode="lines",
                line=list(color="#b48eff",width=2.5,dash="dot"),
                name="所需总样本量", yaxis="y2",
                customdata=round(df_s$hr,3),
                hovertemplate=paste0(
                  "<b>试验组中位：</b>%{x:.1f} 月<br>",
                  "<b>对应 HR：</b>%{customdata:.3f}<br>",
                  "<b>总样本量：</b>%{y}<extra></extra>")) |>
      add_trace(x=cur$median_t, y=cur$nevents,
                type="scatter", mode="markers", yaxis="y",
                marker=list(color="#00c8a0",size=14,symbol="circle",
                            line=list(color="#0f1117",width=2.5)),
                name=sprintf("当前设计·事件=%d",cur$nevents),
                hovertemplate=sprintf(
                  "<b>当前设计</b><br>中位：%.1f 月<br>HR：%.2f<br>事件数：%d<extra></extra>",
                  cur$median_t,cur$hr,cur$nevents)) |>
      add_trace(x=cur$median_t, y=cur$ntotal,
                type="scatter", mode="markers", yaxis="y2",
                marker=list(color="#b48eff",size=12,symbol="diamond",
                            line=list(color="#0f1117",width=2)),
                name=sprintf("当前设计·N=%d",cur$ntotal),
                hovertemplate=sprintf(
                  "<b>当前设计</b><br>中位：%.1f 月<br>HR：%.2f<br>总样本量：%d<extra></extra>",
                  cur$median_t,cur$hr,cur$ntotal))
    
    apply_plotly_theme(p,
                       title    = "样本量与事件数敏感性分析",
                       subtitle = sprintf("对照组中位生存固定为 %.1f 月  ·  试验组范围 %.1f – %.1f 月",
                                          res$median_c,min(mt_range),max(mt_range)),
                       xlab  = "试验组中位生存时间（月）",
                       ylab  = "所需事件数",
                       ylab2 = list(title="所需总样本量",color="#b48eff"))
  })
  
  # ============================================================
  # 图 4：入组 & 事件进度甘特图（汇报用）
  # ============================================================
  output$plot_gantt <- renderPlotly({
    res <- last_result()
    if (is.null(res)) return(no_calc_plotly())
    
    Tr     <- res$Tr
    Ts     <- res$Ts
    T_req  <- res$T_required   # 可能为 NULL
    t_ia   <- res$tIA_f3       # 可能为 NULL
    ev_ia  <- res$events_at_tIA
    ev_tgt <- res$n_events
    n_tot  <- res$n_total
    n_trt  <- ceiling(n_tot*res$ratio/(1+res$ratio))
    n_ctrl <- n_tot - n_trt
    t_end  <- max(Ts, if(!is.null(T_req)) T_req else Ts) * 1.14
    
    p <- plot_ly()
    
    # ━━ 泳道 3：研究总时间线 ━━━━━━━━━━━━━━━━
    # 底层灰条
    p <- p |>
      add_trace(
        x=c(0,Ts,Ts,0,0), y=c(2.62,2.62,3.38,3.38,2.62),
        type="scatter", mode="none", fill="toself",
        fillcolor="rgba(42,47,62,0.5)",
        line=list(color="rgba(42,47,62,0.9)",width=1),
        showlegend=FALSE, hoverinfo="skip") |>
      # 入组期（绿色）
      add_trace(
        x=c(0,Tr,Tr,0,0), y=c(2.68,2.68,3.32,3.32,2.68),
        type="scatter", mode="none", fill="toself",
        fillcolor="rgba(0,200,160,0.35)",
        line=list(color="rgba(0,200,160,0.7)",width=1.5),
        name="入组期",
        hovertemplate=sprintf(
          "<b>入组期</b><br>第 0 – %.0f 月<br>计划入组 %d 例（试验 %d + 对照 %d）<extra></extra>",
          Tr,n_tot,n_trt,n_ctrl)) |>
      # 随访期（蓝色）
      add_trace(
        x=c(Tr,Ts,Ts,Tr,Tr), y=c(2.68,2.68,3.32,3.32,2.68),
        type="scatter", mode="none", fill="toself",
        fillcolor="rgba(78,158,255,0.22)",
        line=list(color="rgba(78,158,255,0.6)",width=1.5),
        name="随访期",
        hovertemplate=sprintf(
          "<b>随访期</b><br>第 %.0f – %.0f 月<br>最短随访 %.0f 月<extra></extra>",
          Tr,Ts,Ts-Tr))
    
    # ━━ 泳道 2：入组进度渐变色块 ━━━━━━━━━━━
    n_seg    <- 24
    t_breaks <- seq(0,Tr,length.out=n_seg+1)
    n_breaks <- round(seq(0,n_tot,length.out=n_seg+1))
    for (i in seq_len(n_seg)){
      frac <- i/n_seg
      r <- as.integer(78+(0-78)*frac)
      g <- as.integer(158+(200-158)*frac)
      b <- as.integer(255+(160-255)*frac)
      fc <- sprintf("rgba(%d,%d,%d,0.55)",r,g,b)
      # 色块本体（不显示hover）
      p <- p |> add_trace(
        x=c(t_breaks[i],t_breaks[i+1],t_breaks[i+1],t_breaks[i],t_breaks[i]),
        y=c(1.68,1.68,2.32,2.32,1.68),
        type="scatter", mode="none", fill="toself",
        fillcolor=fc, line=list(color="rgba(0,0,0,0)"),
        showlegend=FALSE, hoverinfo="skip") |>
        # 不可见中心点，承载 hover 信息
        add_trace(
          x=(t_breaks[i]+t_breaks[i+1])/2, y=2,
          type="scatter", mode="markers",
          marker=list(color="rgba(0,0,0,0)", size=12),
          showlegend=FALSE,
          hovertemplate=sprintf(
            "<b>受试者入组</b><br>时间：%.1f 月<br>累计入组：%d / %d 例<br>完成：%.0f%%<br>试验组：%d 例 / 对照组：%d 例<extra></extra>",
            t_breaks[i+1],n_breaks[i+1],n_tot,frac*100,
            round(n_breaks[i+1]*res$ratio/(1+res$ratio)),
            n_breaks[i+1]-round(n_breaks[i+1]*res$ratio/(1+res$ratio))))
    }
    # 泳道2注释
    p <- p |>
      add_annotations(x=Tr/2, y=2,
                      text=sprintf("<b>受试者入组</b>  0 → %.0f 月  |  N = %d（试验 %d + 对照 %d）",
                                   Tr,n_tot,n_trt,n_ctrl),
                      font=list(color="#d4dbe8",size=12),
                      showarrow=FALSE, xref="x", yref="y",
                      bgcolor="rgba(24,28,39,0.75)", bordercolor="#4e9eff", borderwidth=1)
    
    # ━━ 泳道 1：事件积累渐变色块 ━━━━━━━━━━━
    p <- p |>
      add_trace(
        x=c(0,t_end,t_end,0,0), y=c(0.62,0.62,1.38,1.38,0.62),
        type="scatter", mode="none", fill="toself",
        fillcolor="rgba(42,47,62,0.4)",
        line=list(color="rgba(42,47,62,0.8)",width=1),
        showlegend=FALSE, hoverinfo="skip")
    
    # 事件积累色块（若 T_req 为 NULL 则用 Ts 作为终点）
    t_ev_end <- if (!is.null(T_req)) T_req else Ts
    t_ev  <- seq(0.3, t_ev_end, length.out=40)
    ev_v  <- sapply(t_ev,function(t)
      tryCatch(nEventsIA(x=res$x_surv,tIA=t),error=function(e) NA_real_))
    valid <- !is.na(ev_v)
    t_ev  <- t_ev[valid]; ev_v <- ev_v[valid]
    n_eseg <- length(t_ev)-1
    for (i in seq_len(n_eseg)){
      frac <- min(ev_v[i]/ev_tgt,1)
      r    <- as.integer(180-180*frac)
      g    <- as.integer(142+58*frac)
      b    <- 255L
      fc   <- sprintf("rgba(%d,%d,%d,%.2f)",r,g,b,0.15+0.65*frac)
      # 色块本体（不显示hover）
      p <- p |> add_trace(
        x=c(t_ev[i],t_ev[i+1],t_ev[i+1],t_ev[i],t_ev[i]),
        y=c(0.68,0.68,1.32,1.32,0.68),
        type="scatter", mode="none", fill="toself",
        fillcolor=fc, line=list(color="rgba(0,0,0,0)"),
        showlegend=FALSE, hoverinfo="skip") |>
        # 不可见中心点，承载 hover 信息
        add_trace(
          x=(t_ev[i]+t_ev[i+1])/2, y=1,
          type="scatter", mode="markers",
          marker=list(color="rgba(0,0,0,0)", size=12),
          showlegend=FALSE,
          hovertemplate=sprintf(
            "<b>事件积累进度</b><br>时间：%.1f 月<br>累计事件：%.0f / %d<br>完成：%.1f%%<extra></extra>",
            t_ev[i+1], ev_v[i+1], ev_tgt, ev_v[i+1]/ev_tgt*100))
    }
    
    # ━━ 关键里程碑：按需构建，过滤 NULL ━━━━━━━
    milestones <- list(
      list(t=Tr, lab="入组结束", sub=sprintf("%.0f 月",Tr), col="#00c8a0"),
      list(t=Ts, lab="计划结束", sub=sprintf("%.0f 月",Ts), col="#4e9eff")
    )
    if (!is.null(t_ia) && !is.null(ev_ia)) {
      milestones <- c(milestones, list(
        list(t=t_ia, lab="期中分析",
             sub=sprintf("%.0f 月 · %.0f 事件 (%.0f%%)",t_ia,ev_ia,ev_ia/ev_tgt*100),
             col="#ffb347")))
    }
    if (!is.null(T_req)) {
      milestones <- c(milestones, list(
        list(t=T_req, lab="达到目标",
             sub=sprintf("%.1f 月 · %d 事件",T_req,ev_tgt),
             col="#b48eff")))
      # 若 T_req 与 Ts 极近则去掉"计划结束"避免重叠
      if (abs(Ts-T_req)<0.8) milestones <- milestones[sapply(milestones,function(m) m$lab!="计划结束")]
    }
    # 按时间排序，交替 Y 偏移避免重叠
    milestones <- milestones[order(sapply(milestones,function(m) m$t))]
    ann_y_pool <- c(3.65,3.88,3.65,3.88)
    for (idx in seq_along(milestones)){
      ms <- milestones[[idx]]
      p <- p |>
        add_segments(x=ms$t, xend=ms$t, y=0.55, yend=3.45,
                     line=list(color=ms$col,width=1.5,dash="dot"),
                     showlegend=FALSE,
                     hovertemplate=sprintf("<b>%s</b> · %s<extra></extra>",ms$lab,ms$sub)) |>
        add_annotations(
          x=ms$t, y=ann_y_pool[min(idx,4)],
          text=sprintf("<b>%s</b><br>%s",ms$lab,ms$sub),
          font=list(color=ms$col,size=11),
          showarrow=FALSE, xref="x", yref="y",
          bgcolor="rgba(15,17,23,0.88)", bordercolor=ms$col, borderwidth=1.2)
    }
    
    # ━━ 图例 dummy traces ━━━━━━━━━━━━━━━━━━
    p <- p |>
      add_trace(x=NA,y=NA,type="scatter",mode="markers",
                marker=list(color="#00c8a0",size=10,symbol="square"),
                name="入组期", showlegend=TRUE) |>
      add_trace(x=NA,y=NA,type="scatter",mode="markers",
                marker=list(color="#4e9eff",size=10,symbol="square"),
                name="随访期", showlegend=TRUE) |>
      add_trace(x=NA,y=NA,type="scatter",mode="markers",
                marker=list(color="#b48eff",size=10,symbol="square"),
                name="事件积累进度", showlegend=TRUE)
    
    # ━━ 最终布局 ━━━━━━━━━━━━━━━━━━━━━━━━━
    p |> layout(
      title=list(
        text=paste0(
          "<b>入组 & 事件进度甘特图</b><br>",
          "<sup style='color:#8a96ab'>",
          sprintf("总样本量 %d 例  ·  目标事件 %d  ·  入组 %.0f 月  ·  总研究 %.0f 月",
                  n_tot,ev_tgt,Tr,Ts),
          if(!is.null(T_req)) sprintf("  ·  达到目标预计 %.1f 月",T_req) else "",
          "</sup>"),
        font=list(color="#d4dbe8",size=15,family="'IBM Plex Mono',monospace"),
        x=0.02, xanchor="left", y=0.97, yanchor="top"),
      paper_bgcolor="#0f1117", plot_bgcolor="#181c27",
      font=list(family="'IBM Plex Mono',monospace",color="#d4dbe8",size=12),
      xaxis=list(
        title=list(text="日历时间（月）",
                   font=list(color="#d4dbe8",size=13)),
        gridcolor="#2a2f3e", linecolor="#2a2f3e", zerolinecolor="#2a2f3e",
        tickfont=list(color="#8a96ab",size=11),
        range=c(-0.5,t_end),
        dtick=max(1,round(t_end/10))
      ),
      yaxis=list(
        tickvals=c(1,2,3),
        ticktext=c("事件积累进度","受试者入组","研究总时间线"),
        tickfont=list(color="#d4dbe8",size=13),
        gridcolor="#2a2f3e", linecolor="#2a2f3e",
        range=c(0.3,4.3)
      ),
      legend=list(
        bgcolor="#181c27", bordercolor="#2a2f3e", borderwidth=1,
        font=list(color="#d4dbe8",size=12,family="'IBM Plex Mono',monospace"),
        orientation="h", y=-0.15, x=0, xanchor="left"),
      hoverlabel=list(
        bgcolor="#1e2538", bordercolor="#4e9eff",
        font=list(color="#d4dbe8",size=12,family="'IBM Plex Mono',monospace"),
        align="left"),
      margin=list(l=148,r=40,t=100,b=80)
    ) |>
      config(displayModeBar=TRUE,
             modeBarButtonsToRemove=c("select2d","lasso2d"),
             displaylogo=FALSE,
             toImageButtonOptions=list(format="png",scale=2.5,
                                       filename="trial_gantt"))
  })
  
  # ============================================================
  # 计算结果卡片
  # ============================================================
  output$results_ui <- renderUI({
    if (input$run_btn == 0)
      return(div(class="placeholder",
                 div(class="placeholder-icon", "⬡"),
                 "调整左侧参数，点击「运行计算」"))
    res <- calc_result()
    if (!is.null(res$error))
      return(div(class="error-box", paste0("⚠ 计算错误：", res$error)))
    
    # ── 卡片 03：条件构建 ──────────────────────
    card03 <- if (!is.null(res$tIA_f3) && !is.null(res$events_at_tIA)) {
      div(class="result-card purple",
          div(class="card-tag", paste0("03 · 期中预期事件（tIA=", res$tIA_f3, "月）")),
          div(class="big-stat",
              div(class="big-number", style="color:#b48eff;",
                  sprintf("%.1f", res$events_at_tIA)),
              div(class="big-label",
                  paste0("日历时间 ", res$tIA_f3, " 月时预期累计事件数"))),
          div(class="stat-row",
              span(class="stat-label", "占所需事件数比例"),
              span(class="stat-value highlight-purple",
                   sprintf("%.1f%%", res$events_at_tIA / res$n_events * 100))),
          div(class="stat-row",
              span(class="stat-label", "所需总事件数"),
              span(class="stat-value", res$n_events))
      )
    } else {
      div(class="result-card",
          style="border-top:3px solid #2a2f3e;opacity:0.45;",
          div(class="card-tag", "03 · 期中预期事件"),
          div(style="padding:28px 0;text-align:center;color:#5a6478;font-family:'IBM Plex Mono',monospace;font-size:12px;",
              "← 在左侧填写「指定日历时间」后显示")
      )
    }
    
    # ── 卡片 04：条件构建 ──────────────────────
    card04 <- if (!is.null(res$nevent_f4) && !is.null(res$T_required)) {
      div(style="margin-bottom:16px;",
          div(class="result-card", style="border-top:3px solid var(--accent2);",
              div(class="card-tag",
                  paste0("04 · 达到 ", res$nevent_f4, " 个事件所需日历时间")),
              div(style="display:flex;align-items:center;gap:32px;padding:6px 0;",
                  div(class="big-stat", style="flex:0 0 auto;",
                      div(class="big-number",
                          style="color:var(--accent2);font-size:46px;",
                          sprintf("%.2f", res$T_required)),
                      div(class="big-label", "所需日历时间（月）")),
                  div(style="flex:1;display:grid;grid-template-columns:1fr 1fr;gap:4px 24px;",
                      div(class="stat-row",
                          span(class="stat-label", "目标事件数"),
                          span(class="stat-value", res$nevent_f4)),
                      div(class="stat-row",
                          span(class="stat-label", "占所需事件数比例"),
                          span(class="stat-value highlight-blue",
                               sprintf("%.1f%%", res$nevent_f4 / res$n_events * 100))),
                      div(class="stat-row",
                          span(class="stat-label", "距末例入组后"),
                          span(class="stat-value",
                               sprintf("%.2f 月", max(0, res$T_required - res$Tr)))),
                      div(class="stat-row",
                          span(class="stat-label", "所需总事件数（参考）"),
                          span(class="stat-value", res$n_events))
                  )
              )
          )
      )
    } else {
      div(style="margin-bottom:16px;",
          div(class="result-card",
              style="border-top:3px solid #2a2f3e;opacity:0.45;",
              div(class="card-tag", "04 · 所需日历时间"),
              div(style="padding:28px 0;text-align:center;color:#5a6478;font-family:'IBM Plex Mono',monospace;font-size:12px;",
                  "← 在左侧填写「目标事件数」后显示")
          )
      )
    }
    
    # ── 组合输出 ───────────────────────────────
    tagList(
      div(class="results-grid",
          div(class="result-card green",
              div(class="card-tag", "01 · 样本量计算"),
              div(class="big-stat",
                  div(class="big-number", style="color:var(--accent);", res$n_total),
                  div(class="big-label", "总样本量（例）")),
              div(class="big-divider",
                  lapply(1:20, function(i)
                    tags$span(class=if (length(res$n_events)>0 && length(res$n_total)>0 && res$n_total>0 && i <= round(res$n_events/res$n_total*20)) "filled" else "")
                  )
              ),
              div(class="stat-row",
                  span(class="stat-label", "所需事件数（总）"),
                  span(class="stat-value highlight-green", res$n_events)),
              div(class="stat-row",
                  span(class="stat-label", "事件数：试验组 / 对照组"),
                  span(class="stat-value",
                       paste0(res$ev_trt, " / ", res$ev_ctrl))),
              div(class="stat-row",
                  span(class="stat-label", "样本量：试验组 / 对照组"),
                  span(class="stat-value",
                       paste0(res$n_trt, " / ", res$n_ctrl))),
              div(class="stat-row",
                  span(class="stat-label", "目标把握度"),
                  span(class="stat-value", paste0(round((1-res$beta)*100), "%"))),
              div(class="stat-row",
                  span(class="stat-label", "显著性水平 α"),
                  span(class="stat-value", paste0(res$alpha, "（", res$sided, "侧）"))),
              div(class="stat-row",
                  span(class="stat-label", "计划入组时间"),
                  span(class="stat-value", paste0(res$Tr, " 月"))),
              div(class="stat-row",
                  span(class="stat-label", "末例入组后随访"),
                  span(class="stat-value", paste0(res$minfup, " 月")))
          ),
          div(class="result-card blue",
              div(class="card-tag", "基本设计参数"),
              div(class="big-stat",
                  div(class="big-number", style="color:var(--accent2);",
                      sprintf("%.3f", res$hr)),
                  div(class="big-label", "风险比 HR（对照/试验）")),
              div(class="stat-row",
                  span(class="stat-label", "对照组中位生存"),
                  span(class="stat-value", paste0(res$median_c, " 月"))),
              div(class="stat-row",
                  span(class="stat-label", "试验组中位生存"),
                  span(class="stat-value highlight-blue", paste0(res$median_t, " 月"))),
              div(class="stat-row",
                  span(class="stat-label", "月均入组速率 γ"),
                  span(class="stat-value", sprintf("%.1f", res$gamma))),
              div(class="stat-row",
                  span(class="stat-label", "年化脱落率"),
                  span(class="stat-value", sprintf("%.1f%%", res$eta_year_perc*100))),
              div(class="stat-row",
                  span(class="stat-label", "分配比例（试验:对照）"),
                  span(class="stat-value", paste0(res$ratio, " : 1")))
          )
      ),
      div(class="results-grid",
          div(class="result-card orange",
              div(class="card-tag",
                  paste0("02 · 可检测差异（基于功能1事件数 ", res$n_events, "）")),
              div(class="big-stat",
                  div(class="big-number", style="color:var(--warn);",
                      sprintf("%.4f", res$HR_detectable)),
                  div(class="big-label",
                      paste0("α=", res$alpha, " 下可检出的最大 HR"))),
              div(class="stat-row",
                  span(class="stat-label", "可检出最小试验组中位生存"),
                  span(class="stat-value highlight-orange",
                       paste0(sprintf("%.2f", res$min_surv_test), " 月"))),
              div(class="stat-row",
                  span(class="stat-label", "对照组中位生存（假设不变）"),
                  span(class="stat-value", paste0(res$median_c, " 月"))),
              div(class="stat-row",
                  span(class="stat-label", "功能2分配比例"),
                  span(class="stat-value", sprintf("%.3f", res$ratio_f2)))
          ),
          card03
      ),
      card04
    )
  })
}

shinyApp(ui, server)
