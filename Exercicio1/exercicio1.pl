%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Parte 1

% - Configurações iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% - Definicoes iniciais de entidades presentes na base de conhecimento:

%utente: IdUt, Nome, Idade, Cidade -> {V,F}
%servico: IdServ, Descrição, Instituição, Cidade -> {V,F}
%consulta: Data, IdUt, IdServ, Custo -> {V,F}

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
utente(11,tiagotombado,braga).
utente(8,ricardosquirtle,braga).
utente(69,lucasblastoise,braga).


% - servicos

servico(00001,cardiologia,hospital,braga).
servico(00002,enfermaria,hospital,braga).
servico(00003,psiquiatria,hospital,guimaraes).
servico(00004,podologia,centrodesaude,barcelos).
servico(00005,geneacologia,hospital,guimaraes).
servico(00001,cardiologia,clinica,barcelos).
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

consulta(010218,12345,00001,50).
consulta(200319,69,00005,69).

%--------------------------------- 1.Predicados - - - - - - - - - -  -  -  -  -   -

%--------1.1.Predicado de Registo --------------

registerU(X,Y,Z,W) :- assert(utente(X,Y,Z,W)).
registerU(X,Y,Z,W) :- retract(utente(X,Y,Z,W)),!,fail.

registerS(X,Y,Z,W) :- assert(servico(X,Y,Z,W)).
registerS(X,Y,Z,W) :- retract(servico(X,Y,Z,W)),!,fail.

registerC(X,Y,Z,W) :- assert(consulta(X,Y,Z,W)).
registerC(X,Y,Z,W) :- retract(consulta(X,Y,Z,W)),!,fail.

