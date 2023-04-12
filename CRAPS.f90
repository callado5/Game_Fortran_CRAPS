Integer function lanca_dado() !não possui entrada apenas saída
implicit none
Real:: x 

call random_number(x) !puxa um numero aleatório entre 0 e 1
lanca_dado = INT( 6*x + 1. ) !ajustando o numero aleatório até o [intervalo 1 a 6]

end function lanca_dado
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function Craps()

implicit none
integer :: lanca_dado
integer :: soma, pontos
pontos = 0

!iniciar jogo
write(*,*) 'Jogar CRAPS' 
write(*,*)'(ENTER para lancar os dados)'
read(*,*)
soma = lanca_dado() + lanca_dado()

!Primeira rodada
if (soma == 7 .or. soma ==11) then
    Craps = 1
    write(*,*) 'Voce tirou', soma
    write(*,*) 'Parabens! Voce venceu :D'
else if (soma == 2 .or. soma == 3 .or. soma == 12) then
    Craps = 0
    write(*,*) 'Voce tirou', soma
    write(*,*) 'Sinto muito, voce perdeu :('
!outras rodadas
else
    do 
        pontos = pontos+1
        write(*,*) 'Voce tirou:', soma
        write(*,*) 'Voce tem:', pontos,'pts'
        write(*,*) '(ENTER para lancar os dados)'
        read(*,*)
        soma = lanca_dado() + lanca_dado()
        if (soma == 7 .or. soma == pontos .or.soma == 2 .or. soma == 3 .or. soma == 12) exit

    end do

    if (soma == 7 .or. soma == pontos) then
        Craps = 1
        write(*,*) 'Voce tirou', soma
        write(*,*) 'Parabens! Voce venceu :D'
    else if (soma == 2 .or. soma == 3 .or. soma == 12) then
        write(*,*) 'Voce tirou', soma
        write(*,*) 'Sinto muito, voce perdeu :('
        Craps = 0
    end if
end if

end function Craps
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program Apostas
implicit none
integer :: Craps, jogo
real :: saldo, aposta
character(len=3) :: continuar

saldo = 1000.
jogo = 2
continuar = 'SIM'
!inicio do jogo de apostas

do
    write(*,*) 'Seu saldo e de:',saldo,'reais'
    if (jogo == 1 .or. jogo == 0) then
    write(*,*) 'Deseja continuar?'
    write(*,*) 'digite SIM ou NAO'
    read(*,*) continuar
    end if
    if (continuar == 'NAO' .or. continuar == 'Nao' .or. continuar == 'nao' ) exit
    if (continuar == 'SIM' .or. continuar == 'Sim' .or. continuar == 'sim' ) then
        aposta = 0.
    !condição de falencia
        if (saldo < 0) exit
    !enquanto a aposta for invalida mostre a mensagem abaixo
        do while (aposta > saldo .or. aposta == 0.)
            write(*,*) 'Faca uma aposta, voce tem:',saldo,'reais'
            write(*,*) '(Se a aposta for invalida a mensagem sera repetida)'
            read(*,*) aposta
        end do
    !Iniciar craps 1 = venceu, 0 = perdeu
        jogo = Craps()

        if (jogo > 0.5 ) then !condição de vitória
        saldo =saldo + aposta
        else !Derrota
        saldo = saldo - aposta
        end if   
    else 
        exit
end if
if (saldo <= 0) exit
end do

if (saldo <= 0) then
    write(*,*) 'Lamento, voce Faliu :('
else
    write(*,*) 'Voce terminou com um saldo de:', saldo,'reais'
end if
end program Apostas