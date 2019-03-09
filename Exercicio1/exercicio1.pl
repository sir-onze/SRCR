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

servico(1,cardiologia,hospital,braga).
servico(2,enfermaria,hospital,braga).
servico(3,psiquiatria,hospital,guimaraes).
servico(4,podologia,centrodesaude,barcelos).
servico(5,geneacologia,hospital,guimaraes).
servico(6,cWcac,clinica,barcelos).
servico(7,dermatologia,clinicaprivada,porto).
servico(8,enfermaria,centrodesaude,barcelos).
servico(9,oftalmologia,hospital,guimaraes).
servico(10,fisioterapia,hospital,porto).
servico(11,ortopedia,clinicaprivada,barcelos).
servico(12,enfermaria,hospital,porto).
servico(13,enfermaria,centrodesaude,guimaraes).
servico(14,clinicageral,centrodesaude,braga).
servico(15,urgencia,hospitalprivado,braga).

% - Consultas

consulta(01-02-18,12345,1,50).
consulta(20-03-19,69,5,69).
consulta(02-02-18,12345,1,50).

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


%-------- (v) - Extensão do Predicado de somar todos os elementos de uma lista --------------


sum([],0).
sum([H|T],R) :- sum(T,Y),R is Y+H.

%--------------------------------- Funcionalidades - - - - - - - - - -  -  -  -  -   -




%-------- 1 - Extensão do Predicado que permite registar utentes, serviços e consultas --------------




%-------- 2 - Extensão do Predicado que permite eliminar utentes, serviços e consultas --------------





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


utenteServico(ID,R) :- solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADEU),consulta(DAT,IDU,ID,C)),R).

utenteInstituicao(INSTITUICAO,R) :- solucoes((IDU, NOME, IDADE, CIDADE),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CIDADEU),consulta(DAT,IDU,ID,C)),R).


%-------- 7 - Extensão do Predicado que permite identificar serviços realizados por utente/instituição/cidade --------------


servicoUtente(IDU,R) :- solucoes((ID,DESC,INST,CID),(utente(IDUse,NOME,IDADE,CIDADE),servico(ID,DESC,INST,CID),consulta(DAT,IDU,ID,C)),R).

servicoInstituicao(INSTITUICAO,R) :- solucoes((ID,DESC,INSTITUICAO,CID),(utente(IDU,NOME,IDADE,CIDADE),servico(ID,DESC,INSTITUICAO,CID),consulta(DAT,IDU,ID,C)),R).

servicoCidade(CIDADE,R) :- solucoes((ID,DESC,INST,CIDADE),(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,C)),R).

%-------- 8 - Extensão do Predicado que permite calcular o custo total dos cuidados de saúde por utente/serviço/instituição/data --------------


custoUtente(IDU,R) :- solucoes(Custo,(utente(IDU,NOME,IDADE,CID),servico(ID,DESC,INST,CIDADE),consulta(DAT,IDU,ID,Custo)),R1),sum(R1,R).

