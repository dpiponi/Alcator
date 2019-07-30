        hcli  = $04
        hrdch = $05

.macro  host arg
        .byte   $02, arg
.endmacro

        .segment "UTILITY"
language:
        lda       #<atomcli
        sta       $206
        lda       #>atomcli
        sta       $207
        lda       #<atomrdch
        sta       $20a
        lda       #>atomrdch
        sta       $20b
        rts
atomcli:
        host      hcli
        rts
atomrdch:
        host      hrdch
        bcs       atomrdch
        rts
        .end
