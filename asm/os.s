        hcli  = $04

.macro  host arg
        .byte   $02, arg
.endmacro

        .segment "UTILITY"
language:
        lda       #<atomcli
        sta       $206
        lda       #>atomcli
        sta       $207
        rts
atomcli:
        host      hcli
        rts

        .end
