let init = {|
;; Raster Initialisation
                processor 6502
                org 2049

temp1           = $fb
temp2           = $fc


sys:            
                dc.b $0b,$08
                dc.b $0a,$00
                dc.b $9e
                dc.b $32,$30,$36,$31
                dc.b $00
                dc.b $00,$00

start:          
                jsr $1000

                sei

                lda #<raster
                sta $0314
                lda #>raster
                sta $0315

                lda #50
                sta $d012
                lda $d011
                and #$7f
                sta $d011

                lda #$7f
                sta $dc0d

                lda #$01
                sta $d01a

                lda $dc0d

                cli

                rts


raster:         
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop
                nop

                jsr play

                dec $d019
                jmp $ea31


|}

let playinit = {|
;; SID Initialisation
                org $1000
play_init:      
                ldx #$00

                lda #$00
                sta $d417

                lda #$0f
                sta $d418

                lda #<voice1
                sta v_ptrlo,x
                lda #>voice1
                sta v_ptrhi,x

                inx
                lda #<voice2
                sta v_ptrlo,x
                lda #>voice2
                sta v_ptrhi,x

                inx
                lda #<voice3
                sta v_ptrlo,x
                lda #>voice3
                sta v_ptrhi,x

                rts
|}

let note_initialisation = {|
;; Note Initialisation
gatebit_off:    
                lda #$00
                sta v_waveform,x
                jmp counter_init

note_init:      
                lda v_freqlo_new,x
                beq gatebit_off
                sta v_freqlo,x
                lda v_freqhi_new,x
                sta v_freqhi,x

                ldy v_instr,x

                lda i_pulselo,y
                sta v_pulselo,x
                lda i_pulsehi,y
                sta v_pulsehi,x

                lda i_waveform,y
                sta v_waveform,x

                lda i_ad,y
                sta v_ad,x
                lda i_sr,y
                sta v_sr,x

counter_init:   
                lda v_counternew,x
                sta v_counter,x

                jmp update_sid
|}

let play_loop = {|
;; Main Loop
play:           
                ldx #$00

play_loop:      
                lda v_counter,x
                beq note_init

                cmp #$02
                beq fetch

update_sid:     
                dec v_counter,x
                ldy v_regindex,x

                lda v_freqlo,x
                sta $d400,y
                lda v_freqhi,x
                sta $d401,y

                lda v_pulselo,x
                sta $d402,y
                lda v_pulsehi,x
                sta $d403,y

                lda v_waveform,x
                sta $d404,y

                lda v_ad,x
                sta $d405,y
                lda v_sr,x
                sta $d406,y

                inx
                cpx #$03
                bcc play_loop
                rts
|}

let fetches = {|
;; Fetch Notes
fetch:          
                lda v_ptrlo,x
                sta temp1
                lda v_ptrhi,x
                sta temp2

                ldy #$00

fetch_loop:     
                lda (temp1),y

                cmp #$f9
                bcc fetch_note
                
                iny

                cmp #$fd
                bcc load_instr

                beq jump_addr

                cmp #$fe
                beq enter_seq

                jmp exit_seq

enter_seq:      
                lda temp1

                clc
                adc #$04
                sta v_rtnlo,x

                lda temp2
                adc #$00
                sta v_rtnhi,x

jump_addr       
                lda (temp1),y
                sta v_ptrlo,x

                iny
                lda (temp1),y
                sta v_ptrhi,x

                jmp fetch

exit_seq:       
                lda v_rtnlo,x
                sta v_ptrlo,x
                lda v_rtnhi,x
                sta v_ptrhi,x

                jmp fetch

load_instr:     
                sec
                sbc #$F9
                sta v_instr,x
                jmp fetch_loop

fetch_note:     
                sta v_freqlo_new,x

                iny
                lda (temp1),y
                sta v_freqhi_new,x

                iny
                lda (temp1),y
                sta v_counternew,x

                lda v_waveform,x
                and #$fe
                sta v_waveform,x

                iny
                tya
                clc
                adc temp1
                sta v_ptrlo,x

                lda temp2
                adc #$00
                sta v_ptrhi,x

                jmp update_sid
|}

let voice_data = {|
;; Voice Variables
v_regindex:     dc.b $00,$07,$0E
v_freqlo:       dc.b $00,$00,$00
v_freqlo_new:   dc.b $00,$00,$00
v_freqhi:       dc.b $00,$00,$00
v_freqhi_new:   dc.b $00,$00,$00
v_instr         dc.b $00,$00,$00
v_pulselo       dc.b $00,$00,$00
v_pulsehi       dc.b $00,$00,$00
v_waveform:     dc.b $00,$00,$00
v_ad:           dc.b $00,$00,$00
v_sr:           dc.b $00,$00,$00
v_counter:      dc.b $02,$02,$02
v_counternew:   dc.b $00,$00,$00
v_ptrlo:        dc.b $00,$00,$00
v_ptrhi:        dc.b $00,$00,$00
v_rtnlo:        dc.b $00,$00,$00
v_rtnhi:        dc.b $00,$00,$00
|}

let instrument_data = {|
;; Instrument Variables
i_pulselo:      dc.b $00,$00,$00,$00
i_pulsehi:      dc.b $00,$02,$00,$00
i_pulsespeed:   dc.b $00,$20,$00,$00
i_ad:           dc.b $0a,$09,$58,$0a
i_sr:           dc.b $00,$00,$aa,$f0
i_waveform:     dc.b $81,$41,$21,$11
|}


