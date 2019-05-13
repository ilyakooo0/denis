

--
-- Data for Name: groupChats; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public."groupChats" ("groupChatId", "groupChatUsers", "groupChatName") FROM stdin;
12	{"7": {"isAdmin": false}}	Untitled1
14	{"6": {"isAdmin": false}, "7": {"isAdmin": false}}	new chat
15	{"7": {"isAdmin": false}}	hello
19	{"6": {"isAdmin": false}}
13	{"6": {"isAdmin": true}, "7": {"isAdmin": false}}	Проверка
18	{"3": {"isAdmin": true}, "11": {"isAdmin": false}}	proverka
17	{"6": {"isAdmin": true}, "11": {"isAdmin": false}}	Sosatt
22	{"6": {"isAdmin": true}}	Untitled
23	{"6": {"isAdmin": true}}	Тест
21	{"3": {"isAdmin": true}, "6": {"isAdmin": false}}	new name of chat
20	{"3": {"isAdmin": true}}	Untitled
16	{"6": {"isAdmin": true}}	Hello
\.


--
-- Data for Name: messages; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public.messages ("messageStorageId", "messageStorageAuthorId", "messageStorageDestinationGroupId", "messageStorageDestinationUserId", "messageStorageBody", "messageStorageTime") FROM stdin;
56	\N	12	\N	\N	2019-05-12 19:00:42.217465+00
57	\N	13	\N	\N	2019-05-12 20:00:15.500388+00
60	3	\N	6	{"markdown": "Идёшь завтра на лекцию???"}	2019-05-12 21:16:53.471869+00
61	\N	14	\N	\N	2019-05-12 21:17:28.177618+00
62	3	12	\N	{"markdown": "hello"}	2019-05-12 21:17:38.560284+00
63	\N	15	\N	\N	2019-05-12 21:19:35.607156+00
64	6	\N	3	{"markdown": "Da!"}	2019-05-12 22:22:34.636955+00
69	6	\N	3	{"markdown": "Проверка"}	2019-05-13 09:00:04.543673+00
75	\N	16	\N	\N	2019-05-13 09:42:35.621514+00
76	3	\N	6	{"markdown": "аааа я пишу с сайта "}	2019-05-13 09:51:45.216052+00
77	6	\N	3	{"markdown": ")))"}	2019-05-13 09:59:04.464285+00
78	6	\N	3	{"markdown": "Ты еще можешь отправлять сообщения на ентер"}	2019-05-13 09:59:17.664861+00
79	\N	17	\N	\N	2019-05-13 10:34:51.369108+00
80	12	17	\N	{"markdown": "New chat!"}	2019-05-13 10:34:59.396363+00
81	12	17	\N	{"markdown": "Write me something pls"}	2019-05-13 10:38:06.641916+00
82	6	17	\N	{"markdown": "Hello!"}	2019-05-13 10:39:02.53931+00
83	12	16	\N	{"markdown": "Hello!"}	2019-05-13 10:49:47.834312+00
84	12	17	\N	{"markdown": "Кикнула Илью из беседы)"}	2019-05-13 10:50:45.408526+00
85	12	17	\N	{"markdown": "привет как дела"}	2019-05-13 10:58:54.034185+00
86	6	17	\N	{"markdown": "Хорошо, а у тебя?"}	2019-05-13 11:00:03.129155+00
87	12	\N	3	{"markdown": "Привет!"}	2019-05-13 12:23:21.226991+00
88	12	\N	3	{"markdown": "Как у тебя дела?"}	2019-05-13 12:23:28.143832+00
89	12	17	\N	{"markdown": "Ну так плюс минус жить можно)"}	2019-05-13 12:24:43.553721+00
90	12	17	\N	{"markdown": "привет"}	2019-05-13 13:16:37.040853+00
91	12	17	\N	{"markdown": "Всем пис"}	2019-05-13 13:16:50.7915+00
92	12	\N	3	{"markdown": "Аняяяя"}	2019-05-13 13:20:25.214522+00
93	3	\N	12	{"markdown": "Нормально"}	2019-05-13 14:08:52.660103+00
94	3	\N	12	{"markdown": "Ты как?"}	2019-05-13 14:09:00.358669+00
95	3	\N	6	{"markdown": "Я так и делала!!!"}	2019-05-13 14:09:48.991844+00
96	\N	18	\N	\N	2019-05-13 14:18:30.872479+00
97	\N	19	\N	\N	2019-05-13 15:24:01.710566+00
98	3	19	\N	{"markdown": "hello"}	2019-05-13 15:27:17.523456+00
99	\N	20	\N	\N	2019-05-13 15:27:36.736424+00
100	3	20	\N	{"markdown": "Darya "}	2019-05-13 15:27:53.856998+00
101	3	20	\N	{"markdown": "pls sent message"}	2019-05-13 15:28:18.065107+00
102	\N	21	\N	\N	2019-05-13 17:23:22.627985+00
103	3	21	\N	{"markdown": "hellooooo\\n## markdown"}	2019-05-13 17:27:35.021832+00
104	12	18	\N	{"markdown": "Help"}	2019-05-13 17:40:57.74548+00
105	12	20	\N	{"markdown": "Pls"}	2019-05-13 17:41:28.669253+00
106	12	20	\N	{"markdown": "Send"}	2019-05-13 17:41:33.713239+00
107	12	20	\N	{"markdown": "Nudes"}	2019-05-13 17:41:37.030689+00
108	3	\N	12	{"markdown": "i"}	2019-05-13 18:04:27.566263+00
109	3	\N	12	{"markdown": "need"}	2019-05-13 18:04:29.597069+00
110	3	\N	12	{"markdown": "a"}	2019-05-13 18:04:30.747952+00
111	3	\N	12	{"markdown": "lot"}	2019-05-13 18:04:32.866007+00
112	3	\N	12	{"markdown": "of"}	2019-05-13 18:04:34.439089+00
113	3	\N	12	{"markdown": "messages"}	2019-05-13 18:04:37.348507+00
114	3	\N	12	{"markdown": "lets"}	2019-05-13 18:04:41.274395+00
115	3	\N	12	{"markdown": "check "}	2019-05-13 18:04:43.225609+00
116	3	\N	12	{"markdown": "pagination"}	2019-05-13 18:04:46.724472+00
117	3	\N	12	{"markdown": "))))"}	2019-05-13 18:04:49.117602+00
118	3	\N	12	{"markdown": "Darya"}	2019-05-13 18:04:58.079826+00
119	3	\N	12	{"markdown": "chto delaesh?"}	2019-05-13 18:05:06.691576+00
120	3	\N	12	{"markdown": "native eeeee"}	2019-05-13 18:05:13.633699+00
121	3	21	\N	{"markdown": "блин мне нравится сидеть с сайта"}	2019-05-13 18:38:41.575588+00
122	6	21	\N	{"markdown": "Фига"}	2019-05-13 18:59:23.480114+00
123	6	21	\N	{"markdown": "Неожиданно"}	2019-05-13 18:59:28.098338+00
124	6	21	\N	{"markdown": "А еще мне тоже не хватает смайликов"}	2019-05-13 18:59:36.924505+00
125	6	21	\N	{"markdown": "Надо бы прикрутить))"}	2019-05-13 18:59:41.452505+00
126	12	\N	3	{"markdown": "Привет"}	2019-05-13 19:30:55.962054+00
127	12	\N	3	{"markdown": "У Кости тут время не отображается сообщения, когда ты мне писать то успеваешь!!! _удивлена_ _поражена_ _раздосадована_"}	2019-05-13 19:31:22.510321+00
128	12	\N	3	{"markdown": "привет"}	2019-05-13 19:34:50.753794+00
129	12	\N	3	{"markdown": "блять ну привет ало"}	2019-05-13 19:35:21.876566+00
130	12	\N	3	{"markdown": "Приветттттт!"}	2019-05-13 19:36:01.406531+00
131	12	\N	3	{"markdown": ""}	2019-05-13 19:36:02.600456+00
132	12	\N	3	{"markdown": "привет"}	2019-05-13 19:36:05.515343+00
133	12	\N	3	{"markdown": "где сообщения????"}	2019-05-13 19:36:18.145702+00
134	\N	22	\N	\N	2019-05-13 19:37:11.909828+00
135	\N	23	\N	\N	2019-05-13 19:37:28.304101+00
136	12	\N	11	{"markdown": "Соси писю"}	2019-05-13 19:56:44.176754+00
137	12	\N	3	{"markdown": "Пис"}	2019-05-13 19:58:39.660301+00
138	11	\N	12	{"markdown": "Сама I just want people to be happy "}	2019-05-13 19:59:20.391998+00
139	12	\N	11	{"markdown": "Починила я твои сэйфэриа"}	2019-05-13 20:19:53.326795+00
140	12	\N	11	{"markdown": "Иди и make people happy again сам"}	2019-05-13 20:20:17.66819+00
141	6	23	\N	{"markdown": "Ну что"}	2019-05-13 20:39:46.038071+00
142	6	22	\N	{"markdown": "Тесты"}	2019-05-13 20:39:52.353636+00
143	6	21	\N	{"markdown": "Интеграционные!"}	2019-05-13 20:40:01.990466+00
144	6	19	\N	{"markdown": "Инновационные!"}	2019-05-13 20:40:08.21061+00
145	6	\N	3	{"markdown": "И сюда"}	2019-05-13 20:40:17.047374+00
146	6	16	\N	{"markdown": "Бадумс"}	2019-05-13 20:40:24.582964+00
147	6	17	\N	{"markdown": "Все работает? "}	2019-05-13 20:40:33.688546+00
148	12	\N	11	{"markdown": "Привет"}	2019-05-13 20:47:15.369318+00
149	6	17	\N	{"markdown": "Привет!"}	2019-05-13 20:55:12.148039+00
150	12	\N	12	{"markdown": "пннггргрг"}	2019-05-13 20:59:00.544809+00
154	12	\N	11	{"markdown": "Ильяяяяяяяя"}	2019-05-13 21:02:21.115633+00
155	18	\N	12	{"markdown": "Привеееет"}	2019-05-13 21:16:46.458811+00
156	12	\N	18	{"markdown": "Привет солнце"}	2019-05-13 21:19:24.020172+00
157	11	\N	12	{"markdown": "ильяяяяя"}	2019-05-13 21:36:37.985452+00
158	11	\N	12	{"markdown": "ильяяяяя"}	2019-05-13 21:37:04.22538+00
159	3	\N	6	{"markdown": "Проверка связи"}	2019-05-13 21:40:41.308023+00
160	6	\N	3	{"markdown": "Прием-прием"}	2019-05-13 22:04:37.477219+00
161	3	\N	6	{"markdown": "Костя"}	2019-05-13 22:20:58.83048+00
162	3	\N	6	{"markdown": "Ты сделал коммит?"}	2019-05-13 22:21:14.089104+00
163	3	21	\N	{"markdown": "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of ."}	2019-05-13 22:24:06.104804+00
164	3	21	\N	{"markdown": "### hellllooooo"}	2019-05-13 22:24:16.578672+00
\.


--
-- Data for Name: postElements; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public."postElements" ("rowElementId", "rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment") FROM stdin;
3	0	грустно((((( \n## куда плакать	\N	\N	\N	\N
4	0	я из **Питера** \n_а вы????_	\N	\N	\N	\N
6	0	Я написал длинный пост, а он не отправился, потому что у меня истек токен	\N	\N	\N	\N
8	0	Я придумал! давайте устроим бунт и уроним сервер Илья! 	\N	\N	\N	\N
10	0	Ну сервер не вопротивился, значит можно))\nТипо для привлечения внимания\nВыглядит хначительно серьезнее))	\N	\N	\N	\N
19	0	Тест!	\N	\N	\N	\N
23	0	Привет	\N	\N	\N	\N
24	0	Еще раз	\N	\N	\N	\N
27	0	Новый пост!	\N	\N	\N	\N
28	0	Всем привет	\N	\N	\N	\N
29	0	Привет!	\N	\N	\N	\N
30	0	I am back with information about _UIKit_ !\n\nThe **UIKit** framework provides the required infrastructure for your iOS or tvOS apps. It provides the window and view architecture for implementing your interface, the event handling infrastructure for delivering _Multi-Touch _and other types of input to your app, and the main run loop needed to manage interactions among the user, the system, and your app. Other features offered by the framework include animation support, document support, drawing and printing support, information about the current device, text management and display, search support, accessibility support, app extension support, and resource management.\n	\N	\N	\N	\N
31	0	IOS is the best!I will publish a lot of posts about it! Follow _iOS_ channel with hashtags about _Apple_, _iOS_, _UIKit_!	\N	\N	\N	\N
32	0	Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the\n 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.fjedhfdjdhfdjfd	\N	\N	\N	\N
33	0	San Francisco journalist says police raided his home after he refused to identify source\n\n	\N	\N	\N	\N
34	0	It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like).	\N	\N	\N	\N
35	0	приветик всем, я пишу с сайтика Кости \n\nэто так прикольноооооооооооо, я хочу смайлики((((	\N	\N	\N	\N
36	0	Тоже пишу с сайта Костика!	\N	\N	\N	\N
37	0	# пост про веб и любовь\n\nЯ люблю веб! А вы? _??????_ Я уверена, что тоже!	\N	\N	\N	\N
38	0	У меня что нет постов на страничке??? Не может быть!\n	\N	\N	\N	\N
39	0	Я люблю сосать 	\N	\N	\N	\N
40	0	(очень)	\N	\N	\N	\N
41	0	_Всем привет_!!! Как дела? _Завтра к МК_ :(	\N	\N	\N	\N
42	0	Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!!\n\n# не бейте меня это проверка связи\n\n> quote \n> another quote\n\n_help_\n\n\n\n\n\n\nПривет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!! Привет!!!!\n\n# не бейте меня это проверка связи\n\n> quote \n> another quote\n\n_help_	\N	\N	\N	\N
43	0	Куку\n	\N	\N	\N	\N
44	0	)	\N	\N	\N	\N
45	0	# New post!\n\nThis is an example post about our _Social network_, **iOS** application! \n\nThis is a demonstration of writing a new _post_\n\n> quote can be made like this!\n\n- list\n- is something 🍀\n- like this	\N	\N	\N	\N
46	0	))	\N	\N	\N	\N
47	0	###### очень маленький писюн	\N	\N	\N	\N
48	0	hello, ilya 	\N	\N	\N	\N
49	0	proverka 	\N	\N	\N	\N
\.


--
-- Data for Name: posts; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public.posts ("postRowId", "postRowAuthorId", "postRowUpdateTime", "postRowTags") FROM stdin;
3	3	2019-05-12 11:06:56.113423+00	{hse,new,tag,hello,я,ничего,не,успеваю}
4	3	2019-05-12 11:08:28.56758+00	{Питер,вышка,ура}
6	6	2019-05-12 12:41:48.579615+00	{Злюсь}
8	6	2019-05-12 13:13:59.958842+00	{Бунт,Бунд,Бунт}
10	6	2019-05-12 13:19:59.825686+00	{Бунт,Бунт," Бунт"}
19	6	2019-05-13 08:55:38.703923+00	{Apple}
23	6	2019-05-13 08:58:18.327297+00	{}
24	6	2019-05-13 08:58:57.09688+00	{eb}
27	12	2019-05-13 09:56:38.510814+00	{tag}
28	12	2019-05-13 10:58:05.757737+00	{ag}
29	6	2019-05-13 12:17:40.144256+00	{унт}
30	12	2019-05-13 12:31:11.967519+00	{UIKit,mobileDevelopment,iOS}
31	12	2019-05-13 12:33:09.981899+00	{mobileDevelopment,Swift,iOS}
32	12	2019-05-13 13:27:40.118674+00	{lorem,ipsum}
33	3	2019-05-13 18:09:10.681441+00	{привет,новый,пост,Бунт}
34	3	2019-05-13 18:10:26.800786+00	{tag,new,lorem}
35	3	2019-05-13 18:43:07.731224+00	{obileDevelopment,ышка,ello,Бунт}
36	12	2019-05-13 19:27:48.658511+00	{}
37	12	2019-05-13 19:29:45.334246+00	{web," js",webDev,kosya,ялюблюсвоюработу,веб}
38	12	2019-05-13 19:38:06.900945+00	{}
39	11	2019-05-13 20:02:49.600981+00	{}
40	11	2019-05-13 20:07:02.280734+00	{злюсь}
41	12	2019-05-13 20:52:39.125909+00	{" Бунт"}
42	12	2019-05-13 20:54:41.973395+00	{markdown,help}
43	12	2019-05-13 21:03:30.724701+00	{}
44	6	2019-05-13 21:24:20.968302+00	{orem}
45	12	2019-05-13 21:25:21.929693+00	{social_network,tag,markdown}
46	6	2019-05-13 21:30:40.207788+00	{lorem}
47	11	2019-05-13 21:58:00.669801+00	{}
48	3	2019-05-13 22:17:51.138898+00	{писюн,маленький,Илья}
49	3	2019-05-13 22:28:48.651112+00	{tags,news,hse,web,js,mobileDevelopment,work,kursach}
\.


--
-- Data for Name: quoteElements; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public."quoteElements" ("rowElementId", "rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment") FROM stdin;
\.


--
-- Data for Name: quotes; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public.quotes ("quoteRowId", "quoteRowPostId") FROM stdin;
\.


--
-- Data for Name: tokens; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public.tokens ("tokenUserId", "tokenValue", "tokenExpiryDate", "tokenVerificationCode", "tokenActivationTriesLeft", "tokenUserAgent", "tokenActivationCode", "tokenDeactivationCode") FROM stdin;
17	\\x1a0b89fca490bb64dd1526d2b833079f6954da89e14fb1635623a5800a8c31e25757d75048313d4f07547e228a8971db531faf2be52584307347828ebfc20945	2020-05-12 16:35:45.006489+00	\\xf1fbeea5b543738b22acc2673bbbfa69ea2776f46ba234f85ec251d5dc76287c6351ec553317785c5f23c5bee9c1698a42d27955c5bcaff09305b7ba0ae55310	10	okhttp	\\x0b3e68e5974a5e8b29779e78332278d93b58483f5b06584a4821e1c1ea551ce13a28cd7c9e9350f96577843fe82469bc6bc1876c2ee65337025e4d79dfa7fa2d	\\x0bf0c404a09f2949e9bc26765bdc0671c79865def476cbb2c39de66bcf7c5d386e26db3687ca0d493443c50fbbf68ba0ffe6864abdd3b08b00400024a7852da1
3	\\xd0e9cf3a38ef778783a2d6b4ecc82c30868eed82c62284ed2627eee305c7fbda649e4a40b3e911b7504e18603e8c76ba7291740f3d83200884a3c562256c252a	2020-05-12 18:36:07.869759+00	\N	10	Mac OS X — Chrome	\N	\\x913b36ffad3a049648705f39fee5ffebc9576ce1c59d4aac0558462325b0ef19c6ea17804cc9097af33792baa9655947ed3c275a9f84a5a5d48ab13effeded1a
11	\\x9fb88c39f22896feddaa5e176c6b459701d28f2fa56a83d80b0d8e9fd874618165e934468c2e6932156584daca665b3f41636e4e3dbfd4e9fd70356ede2273d4	2020-05-12 19:57:53.635167+00	\N	10	iOS	\N	\\xe98830312baaa841f4b7b65c799c852fc12f75dd9edecb42a304cebf2dc6ab149a56d9bdacbd16ffb94aff2c188c5af8639e8ff6b9cd18ef34e31e222c35a0a0
12	\\x6c3ea86a478f9f2b3caf61d8bcef018a61b954609e81fb8fdc6dccd9ebce1134e7ef922bbf89c2d274b9cc974b5552b3480042a0980571d79753dcbb143c3d2c	2020-05-12 20:08:01.486261+00	\N	10	iOS	\N	\\x186d1a9dbcd9e3eb35e69b1cb05309a1635d38b8350cfbfbefdc68bd3f37c11af1d9b5670d6c14827e428bcdedfb8f681d9ac3c982d08d8fb29a62f7ebaf810a
6	\\xe10faca3c9403b42d3e5d6ad7dad1bc95be3933b3330b94b55b6f9b6d108cfdac1288e7defbcab6b7e2a94f31756f4f217ed77ae887eac409b268388bc6724cf	2020-05-11 19:57:14.624458+00	\\x5271d7def0733e698a19f9a9378e1b54ba95a8593b2a90a3e404a465f0e8635d56c6894bca0a6eb5c49f5da10ad411e1c316068bb5eed84f4850d71a8a8faf35	10	Windows 10 — Chrome	\\xdf4465993fce0f9ea7b628cd78ab65f1513ee2096139a481b35e7cecc18eacdadaa041bbd2fc5cce11a4137787645e1a6054e98da06777361882d9d952011b91	\\x03f547951f4a8eccc92891efa566f4b9f8370ab41bc428a2c78db322da07efde78b4028a2a0e76e2c92a76e29afdafd97411999fa7b57772f3f3b51fc45a4d7a
6	\\x6ee17d4e1b23c56f626571c1f7ebdf7f2b5b28973402d4e98e8337f43c3b1e73778580616fead50ae62233df4c9bcdcde47e635f0b62bdebd0ebbeebd1151394	2020-05-12 21:06:00.33602+00	\N	10	Windows 10 — Chrome	\N	\\x47015bd8699a424bd41f081dde3bd3122cf6b7b2fde87590ba0eb084196c4f693415d50d8f760af208d8313e9f3e8d8f74aa0fba35d58b6ce69a99d8e33574db
3	\\x4944451363b0a133b458548113ac5091a58a07de228d81e3e4c6345a2f7dec13e3d0a23336460b54991025e4888ceb721e43ca984b456ad0d7b01e5a5bb90bc0	2020-05-12 21:39:15.57724+00	\N	10	okhttp	\N	\\xcca681ba42dd011d288178f9034bf55a5ed4299ccbbe97383774b207f25225559f710e6e587bc8467ad6f09446f4cebca9187ea4cdb144872e6c0bf83128d50f
6	\\x9d74794077a85bde414f93d51c1c2ea12beb41d68ae7053efb0932a2ee0e8da694ab2ce9b4c18823a44df024b7ab7167e81188a360bd051a5f166ce1f5d34866	2020-05-11 19:57:17.459942+00	\N	10	Windows 10 — Chrome	\N	\\x4ee19017009b90b1d8f6e813f5fd5234d29bf0316ac7fc5dfb8504228efe3c100d5364600917dd58317a9a18ba796c3b982df4fa1dd67d9663a7960e7e4e88c4
6	\\x9e7d4f2869f48112728aa46b8c8b757f612b32a62c4081cfe8920977bb2cb87aa966157670fa02255c04e84c66c13eb5a86af54698ce24a2ff0a91157aaebe5c	2020-05-11 14:48:22.634562+00	\N	10	Windows 10 — Chrome	\N	\\x1ce4d8b76b9aa4d528eea1aabe0df7497fd156e0760d0829a30c0963ad252507f1c00a8beaed4648470deccc319caf531e98ebce0b0f653f030bc2c882d7959b
9	\\x8ec80e74ce3720a8028f4f4bcd9c5598ca0343afd63cff5a8933729b1e2e28ca5e1a761420ae898ca51080b8e431ac1eaa5936dcdfaa4a04eeefe016adcdf86a	2020-05-11 15:36:57.456372+00	\\x2c1efa77bd7c3f09179efa261bb10ada6b6b2bdc2aa52f4f7f3f75de3093c0c80afedb8e09441b6fe683e73cd4e28dcdab78cc3d6d8c667c9ce8abb8c7f062d6	10	iOS	\\xf0beaa45dc5b744d1691c0db6389a21e350280c8023a211adb45bcd62328312b89f663fb17452cb1708559eea67895e3d25d55422911053f4b4bb38206f88883	\\x10e95dd6f45681fe8b55e5f43905738a4b2bf49b8b119b559d8aa33090d1141a4071fc67eab7f9426b1932d5674e6690d2b3cd059c12f9b8fdef51f13e708cee
12	\\x7f76401afb1167a59d1aadf3b051900498332e56a909927f004073f676253b65fe8c75860ce8635d7d2e72aeb25f405db4c814304f4ac5120d83d51b8de24e3e	2020-05-12 09:44:34.181474+00	\N	10	iOS	\N	\\xd7132d27c779f16cb32eab613c3716f0436348833fca1ff8ed45a27121a2bd4c6ad4c7cbfaceb588b801708d45bcc578a3dbc19730392fe661d2a37aac086c7e
10	\\xfca54c43c5e2a8e6853d48325cb700a7f50ecbe635fd0a5353a36ea7858ba904a9c712c6a7d2d6e6e8d20aa3aae9f326c0fbe7d8c96f6357295012f5898bea84	2020-05-11 15:52:01.741885+00	\\x3f1fcbaba55307ac237249c0a17d1d2747dfccec6338be65fbefee8b01466f9515c376cedb6951b35a0b286d793284d4277e9e556406f26daae0af69d3362ff9	10	curl	\\x2005dda2be269b6d23c185e0418200f42a17e0ac5b94e8a2a76753470128b1fa2da58b434fa15c2b68228b3212d775fe7a92bf0cb4d4d7dcf1377ed43eb16429	\\x26633323ef3a3c83ff8f4051ebe3d53778a0372534c8bdccfa0538309ee2b545e4baf9f07dfa1ec1728e4ddb95597d033370633a8fab70fc328b2c3932dbfc00
12	\\x3e81a2f178cc25b8280ef5bca9fee6f71e81f00e997a14810d7b6059ed638047aa55c06d9d51aef7eb1732de963ea12a7e12bd3864a3b05f696663f9129c6088	2020-05-12 09:58:42.998106+00	\N	10	iOS	\N	\\x0dc1c1286c3bc853f69ceb5f03dc0480187bd0acb089be8f1aa61dc51d68198cfc017d82ad3c2713e8c67b30e8cfaa81d87969744e231670fc3118a38d89714c
12	\\x25db505e0f4bc136b75ec2ef96132868d31baf8b659ccd89f797ac6d4a6375fa713506a9434d5a4c5f4d6b2663d31dcb6478e3eb92520c9201d6728c315e1424	2020-05-12 10:01:45.081199+00	\\x4c05612fff5c714d46abf6098a308744205ed345003b74d21f9e19622a9544ad5d186f1a84345d2d91dbd1d3a53e0d05461f66439f6b21a7c1c76b483a7b3641	10	iOS	\\x7ebe3eb07ebf2ac66cab89a5fa26f8acc53f3ec06df7bad7522ff58ed5d02086c7f48687bea499b7ce47c04c79245cd06c0c56910f38d9f2b1af265b8de4775b	\\x0a9df3cdc3b1a21eac72e3d77971ada874c899ac14937cb454a1e5c781b1886cfbb8822fb829273da5580ba2b15db947082463cbb36bb050445ea27b15d366a0
12	\\x3f0a881a910c85ea7dfd6ede9f732427bdf08e62b3dd19eb060d4536bf14e153a5804b86a16a61235c22d78dbac959b4fbb8b013d18c656c3d0faa6819727b8c	2020-05-12 10:02:27.63402+00	\N	10	iOS	\N	\\x9581a372265f20f5f51259f89d471e4e14aefe32989f17548832f3a31ec046ce93cc244885c2b7ea607a39249a14643d45b736ad5e786886c9cc2aaf86f89475
12	\\x12e21098059ff5efe4957f54e542fb0d03162f6a1dbdf4881f3d0915afc0e85ac2f29f75a99f706928ba57e77c1ab3b4f6563e4bc740f8dc06589b63774cdc85	2020-05-12 10:56:28.957574+00	\N	10	Mac OS X — Chrome	\N	\\xa61575d8f89b31a6e6ff558c72befaddea92294797433da8cee0d8238953541bf6fe9603f1d9f08fbf1eb094c34ce51de8c20ceb2a0e4704a57473eb6a2d8047
12	\\x239fc16e587f11903a0b983e3e9cc10ce5f9e320b753831190d03f647c55e00ca7f4635211c187a082d827fa5e8c038e40ab0cce47e68ec5a13ab56bc2886259	2020-05-12 19:26:46.204018+00	\\xa0dfb4c9699729277c23f492064763c2dc6496cf8f3c0805b374cffb38eec1f12f3df0c033b832db025a6f5a9deb7d6282fb72526a9e1a55876300848aede8ca	10	Mac OS X — Chrome	\\x55224d5b28d6fa16e692740d8639eb033d142ed1a77ca0e50912acb4c952c9dccbc3616bc6454caebed3f1dbc3a5d5d9b939c7b8956a22b9fbefc3763d1916a0	\\x409a3cfd6904ec3c12b4dd33632040e89f4f340fe90e9b75abb76a787b58311f090204fd718b22e5cf9f6438e1f7984f335142624260f5d0248494ca96ba02b7
11	\\x41da42b65822526a52dec3f651a6d432fa950ddc795fd0a582b5b722b4c6e4956292b329dd58b4471a270a9784e9542f63142d799af8e6c96679e8bd86171835	2020-05-12 20:06:05.904528+00	\N	10	Mac OS X — Chrome	\N	\\xd2af034090688a7c03b7492d69c4bfeacbdf0b9e68be7bd3cc45182a689300dd3c5bb34e38d7e6152057e169b99fc6118cd876ed9987d7ae0e7a69f2dbd82c5c
12	\\x065e8193c701d5e8600eb30f3632845d8ac76f59b81155f88f520ff179bf13a3867e3363fb2589ca93ef438187c1cd990abd3ddd5e126fa722bbe727c13e9348	2020-05-12 20:45:55.846716+00	\N	10	iOS	\N	\\x52274795f5999d01a681f51a5e29afe4edd26889ddaaa77d1d2a85a2bfd08be4af9253d356bb360822034a5171819562177c3cea0e7df6e71212f672fee86f53
18	\\x897a5aa341b7709c7d9cb58f425723458658a1570a26429976b3c92c0da1613fa7e27fbe4a8429b5ab3d4cf2938caf1d730e57911a8b51b2d4f17257473bbd69	2020-05-12 21:13:54.40466+00	\N	10	Mac OS X — Chrome	\N	\\xcae63685d7a24031e550c5e49ac1e8ed8bd3b3bfff51051977c0c2cceba8af2a01faa964cebdac5724c8f0be7ce19f8af7902f6710bb8e989ce35c93833fc9fa
6	\\x6963dfdbf872528ed1f985e4d788cac5b96a9afc5c9e1aa78073e8fca1505a201b459695d3c5dbf8123b8f80f35b7672990389ce26be1bf6873c3fe59f4757ee	2020-05-12 12:37:07.199628+00	\N	10	Windows 10 — Chrome	\N	\\x766516d234fb7cabb224fbd991193784a60d62106e5d4b7ceeb7bdf123d7504a35ca9db3c1e14aacd3bd060c98b3e4e5e4f6bc7e0ee61440437c6464a63eb596
6	\\xef2cec0fe597a7158618a567da72595f81324e5ab42df9ee6f17024f9937267e1b6da7aada49defa7f2938b9e9584aed098087da4796c966fabeaff348d4e5c6	2020-05-12 12:37:55.467622+00	\N	10	Windows 10 — Chrome	\N	\\xc0d6f362c33fc9d3d2e61fef09eed321c7845968e4bd1ae17ddb61128c59861f25db12ac030c5f1871725be3d73e5e68f8ae64a29284cd07a8e1aa693beddf22
6	\\xf26ebf4f5880219a2fc8ae8624adcf8727a3ecd3521f396c811a90ca6ea7c15788b97145e33d6ee6bad7aefca1d53d51c852a4fd951b7204d6ef50b38b3305c6	2020-05-12 12:46:13.117509+00	\N	10	Windows 10 — Chrome	\N	\\x8e0519e8f56e7a4bbab46b58ff498e87a733e6371cd647c597551ad151b3a63c3be4e733d2130bf568a01e59f79c01bf9b38ed6a96940746f8f12ea009019ed3
6	\\x8b48c76b4d6005bf0c02ba7a7b765b6372246e1a7d503b249e901e6ddbade8e1f660272595de6b876afb945cb93cd9e03ab3b0498016e0285864acdb4d1ed228	2020-05-12 12:47:57.324299+00	\N	10	Windows 10 — Chrome	\N	\\xd89f6f8f5c3b9d6c66639febb0d2208023f46851bd402fffb2f86a05684bbde9f2964fdf443405fbf637d51257040e46971194f984927b1801635281497f93b6
6	\\x89fcb726e07982cd2ad61504ed85d2f6ab390108b501ce9533900cc232434409f7c37d66a64fdc48a3b145f110412290bf440d69b3360b323ba9138358d47432	2020-05-12 12:48:18.89163+00	\N	10	Windows 10 — Chrome	\N	\\x8d1344a463e9a6f3f5a334e5bccd532e3d42aed478808c46d0ebf711fcb6eb7e40890b04d713c6d5636e0ac1596c887771100e52b9619cb7aba344119b2adf15
6	\\x2a4e2f5621af53d8aca32b0e6ef9f784661e0d25e9f3c1a6147da28c8ec582b86c1edfd9ad50672156c5b8956eab0f4039ef9f5194c078ae09b6b908997cb828	2020-05-12 12:58:03.390233+00	\N	10	Windows 10 — Chrome	\N	\\xa0fa6918589bee447fa3d63548390836d95571cf94e4056e4cd51cab5f5cdad579774c55385173893bb5cf8f7123ea123fa8de7d6c1fd22bb07fae05eb82c470
6	\\xe79fbed1a6a85dca69674ce3c8f0d04087e3ca08e61a1e6570402405f56f6d7e1ff405c345027645636799e3b67ff432d3885e8f87fd9f37dcbdd509264ab4c9	2020-05-12 13:02:15.709627+00	\N	10	Windows 10 — Chrome	\N	\\xf39f55f18841a4c029d2721dd911e07738011cc0043b783c615d6f68238b60ddc3fc9f13e0f93ca918a81b274c86c6c412b0d1d5d283eea26e5d331ab4eceb63
6	\\x791f98bdab86f17dc631d8a641308babd1bf6a6d80c8d9a13474847b5bdd403a07ceda5bfda647ceeea52cc15d24dcb84417c3900f7744d548f3c11c4927a636	2020-05-12 13:00:39.90702+00	\N	10	Windows 10 — Chrome	\N	\\x54277fb3f03893ee8ca550f6cfd7b2cf03f792ee987dae7a36ecb6b052f5bc46d1262eabc34e2ee6d896f3ba50a9c309cba785ed55928866b1f4d1217e5d62fe
6	\\x8475047b3f4cf52c58d31b5f40f51fbcb421e889b9545af40685b50f77c32060c08adb7593733d174d0d705e7a11b05352745f997e151383c3df4087b87aa437	2020-05-12 13:08:36.061589+00	\N	10	Windows 10 — Chrome	\N	\\x32b6441572986f12c92b4cab97231ed416b0c1538589061bc1498af55b12d6abf16ba10842c97f950ace47bd6a34499a458112de353e003336eaefd87232a441
6	\\xe50a1cabdbce47cd7384ea7e7b9a4ebd1f70bd4d2270f7fd1449243e3c2a514f6374a9415b838a88d44be91260b1c8e789c02451a5215caa823e85ce9a6c6a35	2020-05-12 13:11:01.163827+00	\N	10	Windows 10 — Chrome	\N	\\x4cc5ab60a8b31de5109f9f0f86754e5654aecc8a83b5c553854809690a576d94d71f474d776d490828522db88349c607968d120af014bebdfcc984ff11ec97f3
6	\\x14ecc52de16df124a0cfdd6fac2af58998d7ef75a4b77dc6ffcbebfcb1a0641933125399ccb83ff162b5a4c526cae43989c73db33f5f4e9cd0d667ffa64b1491	2020-05-12 13:15:09.286079+00	\\xa3c9a1ed5ad4cf49cc0d90f9e0f179975cddab69f2306e205a35f1a4daac65dc75cd5182de269d7b1b4f21e6de154279363c31ff7807ee0998e54cc408c542a4	10	Windows 10 — Opera	\\x53b20233e1912eb3e1889f7d62813852a6f00160c923640e0500b3c8056c0e1582cc6ab1f0860c9ea377b7bc057cabc8d1107bfd09a4e1de1a5200facc9e4680	\\x976304336533e75ebd013d336daaec896ea6aac620ba76593cf0f5e9067bbcbd7c22db86c9cf570e555a7d3d38840dea671897363c5de797628db25819cea194
12	\\x9a3f1bc8ad7af3256544d16b10af39f170671b37913ad0795b7c83b92c165815d52e04ae41a778ba1987ceb7ec3771a8a96c2117b1a93e53086a7aa72b744101	2020-05-12 13:17:22.323574+00	\\x68614722f437fd805c9bef73e1319a60ab7ae8d1e669afe8a52c7853d19da646dbd72ad8026329c2555c0b5e5ad4f16793b0a0b55e33f5ca97b941b4d28a03ae	10	Mac OS X — Chrome	\\x03906cccc155cfe4ef4b237e7663f05616623b7a10c2443248f79f3ff9fd3df9c4052107b11e3426a66427b7a22c1205ef6b6b8eaa366500f4ed775281a44bb9	\\x1d33623701873d27f030afc04b3af27554adde549230e814b1791c9104591ca1751e865c89591b689df1e02ffbdfe279fccfd59335d448313a6c7b0c6db34818
12	\\xdb2021edc253bfdf75870708682c0b17c60d9d2552d614da87874341127549d811b49b331f66902e583937282f8b433b64111a28647447bac45457fbc4951397	2020-05-12 13:17:25.803874+00	\N	9	Mac OS X — Chrome	\N	\\x404618e3db88af5f6f48d97526ba2d813ae4cd98943892a3eb33d606182a4c685d1f251c577894aa2d6f192446e6c377250c0f1c199eaae0164e6908921d56d3
6	\\xff236155bcdc965d10d97e37b95498cd48e84f91d84cbf9eeb7149d4488174d0c011ebcec15a708775fc02eb48d987c6a707460cf3f4ad6a5920acfa25e4097d	2020-05-12 13:20:49.70022+00	\N	10	Windows 10 — Chrome	\N	\\xac51b5e531a71c2b90024e308ded6bcf291327ccf58c2a38a79e93cd21aef908469e3ba731d438df0dd830bd9e91a3b4af8baefcf1481539face9f7bd284dc63
6	\\xd8068bbf1c75403f98bc65d5b5023773391eb157e71b1796d5ef8ff6e51e12dfa8eee816dd387a8284e62abf3622bdd62e986ab70d4e7c06eadf0c336c7ddf65	2020-05-12 13:42:52.574211+00	\N	10	Windows 10 — Chrome	\N	\\xfe79ff74735cb836511460da0b64387353ac5d1328f8c429312e09ad27d7d76344ff0ff760beca020bcc651ce6e2014049836bdf8fd2e0b79c752bb58a44edd2
6	\\x4054af8d092a164806b1f5c1a9272fef54d04b14fea1c5c7e1cdbb82541339198bb449f10aeaa6c729b5d60b72201230ccf2cc023d4457cc7990b37b74c53680	2020-05-12 13:43:48.856981+00	\N	10	Windows 10 — Opera	\N	\\x34cf5f64da39a5f9348f024fe04ca22c0f71b85fe3e1538474d1605aef8e65e0dd6b01a31dbb5a6ca5d957edfc746e395a40832406cf6fe3b4bf6f9d67f6d2b1
6	\\xca1eaaff2de79c350e2b452a8adac5d465def68a81b2cae0ce6c6ab06b79fb862594b8557b3d45da17ad2ec2d6cdb19792f6168be7513f30afd709a04bf4a045	2020-05-12 14:12:24.590032+00	\N	10	Windows 10 — Chrome	\N	\\x3099f74e612c1ef8b65d742748232025bdb7c6e54dbcdca70ff015b3f6ed1ed9b6b8b03e5a9cefe80a81598b40e8603ac8d643a4728904a96f8283b4132a670b
12	\\xe9135b7110691c01639db7fe5828c1bbb1fda424d4eb4ede378413493742ef360af3fd97b71f2a99977222725d9e22c8eef62e89331ae2dd269678d8549c44f7	2020-05-12 17:39:51.225664+00	\N	10	iOS	\N	\\x78ccada9b2d8e9a7aa553ad2d1143eff23b8b59caaf3e573cfb115bd66aa2c2d3735fab6bee8fe0c83bc00148129793b806824c2901adc73665841f88b8f15f0
12	\\x4b61df40968e85661eb637ede9aa49bedb12eb9fbaad1250317f102390ae474bc9fc6e49e9f06b61a9b2f79eb9a72c59d0178080d4699abae2b3406bd3702fd0	2020-05-12 19:26:49.449533+00	\N	10	Mac OS X — Chrome	\N	\\xfc0df82243a1174a1c60f066369a22fb75c516e071061dbc539a61cee7853b081dfdb0e6ef05f85cf6a70d9a6a184daa2f8d46de41ae5aab95c0f0f553875411
6	\\x2d1108aa6559686f85b32cb0ed446d42c64021ff43539b4e4462b697dcf2ec1c6c6c306100be9aee0e1321dac27a5dcee76a34abcfda77a7949b01e64a6acdb6	2020-05-12 14:22:04.298934+00	\N	10	Windows 10 — Chrome	\N	\\x347810d21c6b1c39fc8deb5056df65f5aff393a8b94b4f9fc80064a5d4e3b98d3b6de9ea8e2fcc0625ede95a3cf94f707c490710da086d6dfe645f6bc498d814
11	\\xd0c382a30c6508107c4095fbe6713c8b987d51145e5e95b25fe082d00c89af26c9d4f46b5c6a9a63bbc82a63a57c9c0d488cb6787f64465a0b29dbe4b481aec1	2020-05-12 20:06:08.427816+00	\N	10	Mac OS X — Chrome	\N	\\xc7d039fa0c13356ffd1379a8ec10a97b3bbfafb0a3222aaabff85c5467ffb8224bbf1e450aafd278642aa457db904d4318d05a37a3ad65b6e42055729baa992c
6	\\x783d36a882b22f9e35e0e67f16058bb01d07bbaee4f35465f84125868367161aa77a48c24fce9a8587ab5baeaefc7f20d2d1fbe6b9ffc3e7ea079ce4b4fc2065	2020-05-12 21:05:34.945924+00	\\xaed141900570ffbc6027328ebaedc19516014fd15e51f2dac2f63d0ebdcc0b6e42893be3ad9edca85b521412357bb0e2ec74eb359af17b6d7d5bbac9b59912e4	10	Windows 10 — Chrome	\\xa17c601d49975527b36bdeccfedca7f66bbc0603ed8456fe99c8592e270f070ab15dd083efc278665c295af2667b345c24f1da9d1d19299bbd001971b4e3f112	\\xe887bda8dc9e076244c88d678885f81b21b93d228fe24cd0f8619203671e40e0db8d4f50168083f1742809cb75338ddc62c08ba017790ea6f15602ef2fa77fac
11	\\x467a1e43d01b6d92be00e984d74d2984225cead7380ddba3a3b962b64fa7c99878cfda33c15bc72826fd4d747c05b1905a51fcecd7bdc2c20322c1c9c0cde1b8	2020-05-12 21:35:38.166955+00	\N	10	Mac OS X — Chrome	\N	\\xab87612391d51e870bce7b33ed618fbc1310cf2bfffe7cb8a887a161a107187924848e0f9aab60fd2719155984a800181a4d1dc7b58680da197b2b1e373df210
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: qsfrneimsrtpjz
--

COPY public.users ("userRowId", "userRowFirstName", "userRowMiddleName", "userRowLastName", "userRowFacultyUrl", "userRowEmail", "userRowIsValidated") FROM stdin;
12	Дарья	Юрьевна	Редникина	strategy.hse.ru/	dyurednikina@edu.hse.ru	t
18	Darya	_	_	cs.hse.ru/dse/	rednikina.com@yandex.ru	t
6	Костя	Игоревич	Манежин	perm.hse.ru/sgf/hum/	kimanezhin@edu.hse.ru	t
11	Костюченко	Игоревич	Илья	spb.hse.ru/fmcs/math/	iikostyuchenko@edu.hse.ru	t
3	Анна	Денисовна	Михалева	spb.hse.ru/humart/design/	admikhaleva@edu.hse.ru	t
9	Darya	Yuryevna	Rednikina	cs.hse.ru/dse/	dyrednikina@edu.hse.ru	f
17	anna	denisovna	mikhaleva	scem.spb.hse.ru/man/	hello@edu.hse.ru	f
10	Seva	Algebrovich	Leonidov	cs.hse.ru/dse/	iikostyuchen@edu.hse.ru	f
\.


--
-- Name: channels_namedChannelFullId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."channels_namedChannelFullId_seq"', 37, true);


--
-- Name: groupChats_groupChatId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."groupChats_groupChatId_seq"', 24, true);


--
-- Name: messages_messageStorageId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."messages_messageStorageId_seq"', 164, true);


--
-- Name: posts_postRowId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."posts_postRowId_seq"', 49, true);


--
-- Name: quotes_quoteRowId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."quotes_quoteRowId_seq"', 1, false);


--
-- Name: users_userRowId_seq; Type: SEQUENCE SET; Schema: public; Owner: qsfrneimsrtpjz
--

SELECT pg_catalog.setval('public."users_userRowId_seq"', 18, true);


--
-- Name: channels pk_channels; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT pk_channels PRIMARY KEY ("namedChannelFullId");


--
-- Name: faculties pk_faculties; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.faculties
    ADD CONSTRAINT pk_faculties PRIMARY KEY ("facultyUrl");


--
-- Name: groupChats pk_group_chats; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public."groupChats"
    ADD CONSTRAINT pk_group_chats PRIMARY KEY ("groupChatId");


--
-- Name: messages pk_messages; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.messages
    ADD CONSTRAINT pk_messages PRIMARY KEY ("messageStorageId");


--
-- Name: quotes pk_post_quotes; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.quotes
    ADD CONSTRAINT pk_post_quotes PRIMARY KEY ("quoteRowId");


--
-- Name: posts pk_posts; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT pk_posts PRIMARY KEY ("postRowId");


--
-- Name: users pk_users; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT pk_users PRIMARY KEY ("userRowId");


--
-- Name: users user_email; Type: CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT user_email UNIQUE ("userRowEmail");


--
-- Name: posts fk_author_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT fk_author_id FOREIGN KEY ("postRowAuthorId") REFERENCES public.users("userRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: channels fk_channels_users_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT fk_channels_users_id FOREIGN KEY ("namedChannelFullOwner") REFERENCES public.users("userRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: messages fk_messages_author; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.messages
    ADD CONSTRAINT fk_messages_author FOREIGN KEY ("messageStorageAuthorId") REFERENCES public.users("userRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: messages fk_messages_group; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.messages
    ADD CONSTRAINT fk_messages_group FOREIGN KEY ("messageStorageDestinationGroupId") REFERENCES public."groupChats"("groupChatId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: messages fk_messages_to_user; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.messages
    ADD CONSTRAINT fk_messages_to_user FOREIGN KEY ("messageStorageDestinationUserId") REFERENCES public.users("userRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: postElements fk_post_quote_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public."postElements"
    ADD CONSTRAINT fk_post_quote_id FOREIGN KEY ("rowElementQuote") REFERENCES public.quotes("quoteRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: postElements fk_post_quote_self_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public."postElements"
    ADD CONSTRAINT fk_post_quote_self_id FOREIGN KEY ("rowElementId") REFERENCES public.posts("postRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: quotes fk_quote_post_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.quotes
    ADD CONSTRAINT fk_quote_post_id FOREIGN KEY ("quoteRowPostId") REFERENCES public.posts("postRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: quoteElements fk_quote_quote_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public."quoteElements"
    ADD CONSTRAINT fk_quote_quote_id FOREIGN KEY ("rowElementQuote") REFERENCES public.quotes("quoteRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: quoteElements fk_quotes_quote_self_id; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public."quoteElements"
    ADD CONSTRAINT fk_quotes_quote_self_id FOREIGN KEY ("rowElementId") REFERENCES public.quotes("quoteRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: users fk_user_faculty; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT fk_user_faculty FOREIGN KEY ("userRowFacultyUrl") REFERENCES public.faculties("facultyUrl") ON UPDATE CASCADE ON DELETE RESTRICT;


--
-- Name: tokens fk_user_token; Type: FK CONSTRAINT; Schema: public; Owner: qsfrneimsrtpjz
--

ALTER TABLE ONLY public.tokens
    ADD CONSTRAINT fk_user_token FOREIGN KEY ("tokenUserId") REFERENCES public.users("userRowId") ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: LANGUAGE plpgsql; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON LANGUAGE plpgsql TO qsfrneimsrtpjz;


--
-- PostgreSQL database dump complete
--

