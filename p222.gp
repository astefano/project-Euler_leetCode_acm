? s = 50 + 2*sqrt(50*48)
%21 = 147.9795897113271239278913630
? forstep(i=48, 32, -2, s += sqrt(100*(2*(i + i-2) - 100)))
? s
%22 = 809.0025083423618758124361651
? forstep(i=31, 47, 2, s += sqrt(100*(2*(i + i+2) - 100)))
? s
%23 = 1495.028958552727681175614410
? s + sqrt(100*(2*61-100)) + 49
%24 = 1590.933116150961976721270711

am trisat... am folosit secventa 50, 48, 46, ..., 32, 30, 31, ..., 47, 49 de la http://math.stackexchange.com/questions/97842/project-euler-question-222

