%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Parte 1

% - Configurações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% - definição de invariante

:- op(900,xfy,'::').

% - Definicoes iniciais de entidades presentes na base de conhecimento:

%utente: IdUt, Nome, Idade, Cidade -> {V,F}
%servico: IdServ, Descrição, Instituição, Cidade -> {V,F}
%consulta: Data, IdUt, IdServ, Custo -> {V,F}
%instituicao: IdIn,Nome,Cidade -> {V,F}

% - Opções para permitirem inserções e remoções na base de conhecimento

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.

%--------------------------------- Base de conhecimento - - - - - - - - - -  -  -  -  -   -
% - Utentes

utente(12345,joaocarlos,20,braga). 
utente(67810,martagalo,20,barcelos).
utente(11123,carlamuamba,20,luanda).
utente(14156,pedroavec,20,paris).
utente(17189,joaodostrespes,20,braga).
utente(20212,ricardoultra,20,guimaraes).
utente(23245,joanatripeira,20,porto).
utente(26278,joaodasfrigideiras,20,braga).
utente(28293,margaridaleao,20,braga).
utente(31323,fredericasantos,20,beja).
utente(33345,paulotabuada,20,sintra).
utente(51523,adrianodovitoria20,aveiro).
utente(11,tiagotombado,22,braga).
utente(8,ricardosquirtle,22,braga).
utente(69,lucasblastoise,23,braga).


% - servicos

servico(00001,cardiologia,hospital,braga).
servico(00002,enfermaria,hospital,braga).
servico(00003,psiquiatria,hospital,guimaraes).
servico(00004,podologia,centrodesaude,barcelos).
servico(00005,geneacologia,hospital,guimaraes).
servico(00001,cWcac,clinica,barcelos).
servico(00007,dermatologia,clinicaprivada,porto).
servico(00002,enfermaria,centrodesaude,barcelos).
servico(00009,oftalmologia,hospital,guimaraes).
servico(00010,fisioterapia,hospital,porto).
servico(00011,ortopedia,clinicaprivada,barcelos).
servico(00011,enfermaria,hospital,porto).
servico(00002,enfermaria,centrodesaude,guimaraes).
servico(00014,clinicageral,centrodesaude,braga).
servico(00015,urgencia,hospitalprivado,braga).

% - Consultas

consulta(01-02-18,12345,00001,50).
consulta(20-03-19,69,00005,69).

%--------------------------------- Predicados auxiliares - - - - - - - - - -  -  -  -  -   -

%-------- (i) - Extensão do Predicado de inserção  --------------


insere(X) :- assert(X).
insere(X) :- retract(X),!,fail.


%-------- (ii) - Extensão do Predicado de Remoção  --------------


remove(X) :- retract(X).
remove(X) :- assert(X),!,fail.


%-------- (iii) - Extensão do Predicado de encontrar todas as soluções  --------------


solucoes(T,Q,S):- 
        findall(T,Q,S).


%-------- (iv) - Extensão do Predicado de remover repetidos  --------------

remOne(X,[],[]).
remOne(X,[X|XS],XS).
remOne(X,[Q|XS],[Y|YS]) :- remOne(X,XS,YS).



%-------- 1 - Extensão do Predicado que permite Registarutentes, serviçose consultas --------------



%-------- 2 - Extensão do Predicado que permite Registarutentes, serviçose consultas --------------





%-------- 3 - Extensão do Predicado que permite identificar todas as Instituições prestadoras de servico --------------






%-------- 4 - Extensão do Predicado que permite identificar utentes por id e nome,servico por id e consulta por id de servico e id de utente--------------


idUtente(ID,R) :- solucoes((ID,N,I,C), utente(ID,N,I,C), R).

nomeUtente(NOME,R) :- solucoes((ID,NOME,I,C),utente(ID,NOME,I,C),R).

idServ(ID,R) :- solucoes((ID,SERV,INS,LOC),servico(ID,SERV,INS,LOC)).

servConsulta(ID,R) :- solucoes((DAT,UT,ID,C), consulta(DAT,UT,ID,C), R).

utenteConsulta(ID,R) :- solucoes((DAT,ID,SER,C),consulta(DAT,ID,SER,C),R). 


%-------- 5 - Extensão do Predicado que permite Identificar serviços prestados por instituição/cidade/datas/custo --------------


instServico(INSTITUICAO,R) :- solucoes((ID,DESC,INSTITUICAO,C),servico(ID,DESC,INSTITUICAO,C),R).

cidadeServico(CIDADE,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADE),R).

dataServico(DAT,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(DAT,UT,ID,C),servico(ID,DESC,INSTITUICAO,CIDADE)),R).

custoServico(CUSTO,R) :- solucoes((ID,DESC,INSTITUICAO,CIDADE),(consulta(DAT,UT,ID,CUSTO),servico(ID,DESC,INSTITUICAO,CIDADE)),R).


%-------- 6 - Extensão do Predicado que permite identificar os utentes de um serviço/instituição --------------



%-------- 7 - Extensão do Predicado que permite identificar serviços realizados por utente/instituição/cidade --------------







