Config { font = "xft:monospace:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = ["xft:FontAwesome:pixelsize=15"]
       , bgColor = "#282A36"
       , fgColor = "#F8F8F2"
       , position = TopP 8 8
       , lowerOnStart = True
       , hideOnStart = False
       , persistent = True
       , commands = [ Run Volume "default" "Master" [ "--template", "Vol <volumestatus>%"
                                                    , "--High", "75"
                                                    , "--high", "#FF5555"
                                                    ] 10
                    , Run Cpu [ "--template", "Cpu <total>%"
                              , "--Low",      "3"
                              , "--High",     "75"
                              , "--normal",   "#50FA7B"
                              , "--high",     "#FF5555"
                              ] 10
                    , Run Memory [ "--template", "Mem <usedratio>%"
                                 , "--Low",      "50"
                                 , "--High",     "80"
                                 , "--low",      "#50FA7B"
                                 , "--normal",   "#F1FA8C"
                                 , "--high",     "#FF5555"
                                 ] 10
                    , Run DynNetwork [ "--template", "<rx>kB/s <tx>kB/s"
                                     , "--High",   "20000000" -- In B/s
                                     , "--normal", "#50FA7B"
                                     , "--high",   "#FF5555"
                                     ] 10
                    , Run Date "%d %b %Y (%a)" "date" 10
                    , Run Date "%R" "clock" 10
                    , Run StdinReader ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %updates% : %default:Master% : %dynnetwork% : %cpu% : %memory% : %date% : %clock%"
       }
