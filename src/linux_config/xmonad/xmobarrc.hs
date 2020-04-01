Config { font = "xft:monospace:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = ["xft:FontAwesome:pixelsize=15"]
       , bgColor = "#282A36"
       , fgColor = "#F8F8F2"
       , position = TopP 8 8
       , lowerOnStart = True
       , hideOnStart = False
       , persistent = True
       , commands = [ Run Alsa "default" "Master" [ "--template", "Vol <volume>% <status>"
                                                  , "--High", "75"
                                                  , "--high", "#FF5555"
                                                  , "--"
                                                  , "--onc",  "#50FA7B"
                                                  , "--offc", "#FF5555"
                                                  ]
                    , Run Cpu [ "--template", "Cpu <total>%"
                              , "--High",     "75"
                              --, "--normal",   "#BD93F9"
                              , "--high",     "#FF5555"
                              ] 10
                    , Run Memory [ "--template", "Mem <usedratio>%"
                                 , "--Low",      "50"
                                 , "--High",     "80"
                                 , "--low",      "#F1FA8C"
                                 , "--normal",   "#F1FA8C"
                                 , "--high",     "#FF5555"
                                 ] 10
                    , Run DynNetwork [ "--template", "<rx>kB/s <tx>kB/s"
                                     , "--High",   "20000000" -- In B/s
                                     , "--normal", "#FF79C6"
                                     , "--high",   "#FF5555"
                                     ] 10
                    , Run Date "%d %b %Y (%a) %R" "date" 10
                    , Run StdinReader ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#8BE9FD>%alsa:default:Master%</fc> : <fc=#FF79C6>%dynnetwork%</fc> : <fc=#BD93F9>%cpu%</fc> : <fc=#F1FA8C>%memory%</fc> : <fc=#50FA7B>%date%</fc>"
       }
