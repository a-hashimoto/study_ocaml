type ekimei_t = { 
  kanji   : string; (* 駅名 *) 
  kana    : string; (* 読み *) 
  romaji  : string; (* ローマ字 *) 
  shozoku : string; (* 所属線名 *) 
} 
 
type ekikan_t = { 
  kiten  : string; (* 起点 *) 
  shuten : string; (* 終点 *) 
  keiyu  : string; (* 経由線名 *) 
  kyori  : float;  (* 距離 *) 
  jikan  : int;    (* 時間 *) 
} 

(* 12-1 *)
type eki_t = {
   namae : string; (* 駅名（漢字の文字列）*) 
   saitan_kyori : float; (* 最短距離 *)
   temae_list : string list; (* 駅名のリスト *)
}
 
let global_ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}; 
{kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"}; 
{kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"}; 
{kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidoumae"; shozoku="千代田線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="千代田線"}; 
{kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="千代田線"}; 
{kanji="二重橋前"; kana="にじゅうばしまえ"; romaji="nijuubasimae"; shozoku="千代田線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="千代田線"}; 
{kanji="新御茶ノ水"; kana="しんおちゃのみず"; romaji="shin-ochanomizu"; shozoku="千代田線"}; 
{kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"}; 
{kanji="根津"; kana="ねづ"; romaji="nedu"; shozoku="千代田線"}; 
{kanji="千駄木"; kana="せんだぎ"; romaji="sendagi"; shozoku="千代田線"}; 
{kanji="西日暮里"; kana="にしにっぽり"; romaji="nishinippori"; shozoku="千代田線"}; 
{kanji="町屋"; kana="まちや"; romaji="machiya"; shozoku="千代田線"}; 
{kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"}; 
{kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"}; 
{kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"}; 
{kanji="浅草"; kana="あさくさ"; romaji="asakusa"; shozoku="銀座線"}; 
{kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"}; 
{kanji="稲荷町"; kana="いなりちょう"; romaji="inaricho"; shozoku="銀座線"}; 
{kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="銀座線"}; 
{kanji="上野広小路"; kana="うえのひろこうじ"; romaji="uenohirokoji"; shozoku="銀座線"}; 
{kanji="末広町"; kana="すえひろちょう"; romaji="suehirocho"; shozoku="銀座線"}; 
{kanji="神田"; kana="かんだ"; romaji="kanda"; shozoku="銀座線"}; 
{kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="銀座線"}; 
{kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="銀座線"}; 
{kanji="京橋"; kana="きょうばし"; romaji="kyobashi"; shozoku="銀座線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="銀座線"}; 
{kanji="新橋"; kana="しんばし"; romaji="shinbashi"; shozoku="銀座線"}; 
{kanji="虎ノ門"; kana="とらのもん"; romaji="toranomon"; shozoku="銀座線"}; 
{kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesannou"; shozoku="銀座線"}; 
{kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="銀座線"}; 
{kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyamaicchome"; shozoku="銀座線"}; 
{kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"}; 
{kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"}; 
{kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"}; 
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="半蔵門線"}; 
{kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"}; 
{kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"}; 
{kanji="半蔵門"; kana="はんぞうもん"; romaji="hanzomon"; shozoku="半蔵門線"}; 
{kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="半蔵門線"}; 
{kanji="神保町"; kana="じんぼうちょう"; romaji="jinbocho"; shozoku="半蔵門線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="半蔵門線"}; 
{kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="半蔵門線"}; 
{kanji="水天宮前"; kana="すいてんぐうまえ"; romaji="suitengumae"; shozoku="半蔵門線"}; 
{kanji="清澄白河"; kana="きよすみしらかわ"; romaji="kiyosumi-shirakawa"; shozoku="半蔵門線"}; 
{kanji="住吉"; kana="すみよし"; romaji="sumiyoshi"; shozoku="半蔵門線"}; 
{kanji="錦糸町"; kana="きんしちょう"; romaji="kinshicho"; shozoku="半蔵門線"}; 
{kanji="押上"; kana="おしあげ"; romaji="oshiage"; shozoku="半蔵門線"}; 
{kanji="中目黒"; kana="なかめぐろ"; romaji="nakameguro"; shozoku="日比谷線"}; 
{kanji="恵比寿"; kana="えびす"; romaji="ebisu"; shozoku="日比谷線"}; 
{kanji="広尾"; kana="ひろお"; romaji="hiro"; shozoku="日比谷線"}; 
{kanji="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; shozoku="日比谷線"}; 
{kanji="神谷町"; kana="かみやちょう"; romaji="kamiyacho"; shozoku="日比谷線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="日比谷線"}; 
{kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="日比谷線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="日比谷線"}; 
{kanji="東銀座"; kana="ひがしぎんざ"; romaji="higashiginza"; shozoku="日比谷線"}; 
{kanji="築地"; kana="つきじ"; romaji="tsukiji"; shozoku="日比谷線"}; 
{kanji="八丁堀"; kana="はっちょうぼり"; romaji="hacchobori"; shozoku="日比谷線"}; 
{kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="日比谷線"}; 
{kanji="人形町"; kana="にんぎょうちょう"; romaji="ningyomachi"; shozoku="日比谷線"}; 
{kanji="小伝馬町"; kana="こでんまちょう"; romaji="kodemmacho"; shozoku="日比谷線"}; 
{kanji="秋葉原"; kana="あきはばら"; romaji="akihabara"; shozoku="日比谷線"}; 
{kanji="仲御徒町"; kana="なかおかちまち"; romaji="nakaokachimachi"; shozoku="日比谷線"}; 
{kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="日比谷線"}; 
{kanji="入谷"; kana="いりや"; romaji="iriya"; shozoku="日比谷線"}; 
{kanji="三ノ輪"; kana="みのわ"; romaji="minowa"; shozoku="日比谷線"}; 
{kanji="南千住"; kana="みなみせんじゅ"; romaji="minamisenju"; shozoku="日比谷線"}; 
{kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenju"; shozoku="日比谷線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
{kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"}; 
{kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"}; 
{kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"}; 
{kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"}; 
{kanji="淡路町"; kana="あわじちょう"; romaji="awajicho"; shozoku="丸ノ内線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="丸ノ内線"}; 
{kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="丸ノ内線"}; 
{kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"}; 
{kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="丸ノ内線"}; 
{kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; shozoku="丸ノ内線"}; 
{kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"}; 
{kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="丸ノ内線"}; 
{kanji="四谷三丁目"; kana="よつやさんちょうめ"; romaji="yotsuyasanchome"; shozoku="丸ノ内線"}; 
{kanji="新宿御苑前"; kana="しんじゅくぎょえんまえ"; romaji="shinjuku-gyoemmae"; shozoku="丸ノ内線"}; 
{kanji="新宿三丁目"; kana="しんじゅくさんちょうめ"; romaji="shinjuku-sanchome"; shozoku="丸ノ内線"}; 
{kanji="新宿"; kana="しんじゅく"; romaji="shinjuku"; shozoku="丸ノ内線"}; 
{kanji="西新宿"; kana="にししんじゅく"; romaji="nishi-shinjuku"; shozoku="丸ノ内線"}; 
{kanji="中野坂上"; kana="なかのさかうえ"; romaji="nakano-sakaue"; shozoku="丸ノ内線"}; 
{kanji="新中野"; kana="しんなかの"; romaji="shin-nakano"; shozoku="丸ノ内線"}; 
{kanji="東高円寺"; kana="ひがしこうえんじ"; romaji="higashi-koenji"; shozoku="丸ノ内線"}; 
{kanji="新高円寺"; kana="しんこうえんじ"; romaji="shin-koenji"; shozoku="丸ノ内線"}; 
{kanji="南阿佐ヶ谷"; kana="みなみあさがや"; romaji="minami-asagaya"; shozoku="丸ノ内線"}; 
{kanji="荻窪"; kana="おぎくぼ"; romaji="ogikubo"; shozoku="丸ノ内線"}; 
{kanji="中野新橋"; kana="なかのしんばし"; romaji="nakano-shimbashi"; shozoku="丸ノ内線"}; 
{kanji="中野富士見町"; kana="なかのふじみちょう"; romaji="nakano-fujimicho"; shozoku="丸ノ内線"}; 
{kanji="方南町"; kana="ほうなんちょう"; romaji="honancho"; shozoku="丸ノ内線"}; 
{kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="南北線"}; 
{kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="南北線"}; 
{kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesanno"; shozoku="南北線"}; 
{kanji="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; romaji="roppongiitchome"; shozoku="南北線"}; 
{kanji="麻布十番"; kana="あざぶじゅうばん"; romaji="azabujuban"; shozoku="南北線"}; 
{kanji="白金高輪"; kana="しろかねたかなわ"; romaji="shirokanetakanawa"; shozoku="南北線"}; 
{kanji="白金台"; kana="しろかねだい"; romaji="shirokanedai"; shozoku="南北線"}; 
{kanji="目黒"; kana="めぐろ"; romaji="meguro"; shozoku="南北線"}; 
{kanji="市ヶ谷"; kana="いちがや"; romaji="ichigaya"; shozoku="南北線"}; 
{kanji="飯田橋"; kana="いいだばし"; romaji="idabashi"; shozoku="南北線"}; 
{kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="南北線"}; 
{kanji="東大前"; kana="とうだいまえ"; romaji="todaimae"; shozoku="南北線"}; 
{kanji="本駒込"; kana="ほんこまごめ"; romaji="honkomagome"; shozoku="南北線"}; 
{kanji="駒込"; kana="こまごめ"; romaji="komagome"; shozoku="南北線"}; 
{kanji="西ヶ原"; kana="にしがはら"; romaji="nishigahara"; shozoku="南北線"}; 
{kanji="王子"; kana="おうじ"; romaji="oji"; shozoku="南北線"}; 
{kanji="王子神谷"; kana="おうじかみや"; romaji="ojikamiya"; shozoku="南北線"}; 
{kanji="志茂"; kana="しも"; romaji="shimo"; shozoku="南北線"}; 
{kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"}; 
{kanji="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; shozoku="東西線"}; 
{kanji="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; shozoku="東西線"}; 
{kanji="妙典"; kana="みょうでん"; romaji="myoden"; shozoku="東西線"}; 
{kanji="行徳"; kana="ぎょうとく"; romaji="gyotoku"; shozoku="東西線"}; 
{kanji="南行徳"; kana="みなみぎょうとく"; romaji="minami-gyotoku"; shozoku="東西線"}; 
{kanji="浦安"; kana="うらやす"; romaji="urayasu"; shozoku="東西線"}; 
{kanji="葛西"; kana="かさい"; romaji="kasai"; shozoku="東西線"}; 
{kanji="西葛西"; kana="にしかさい"; romaji="nishi-kasai"; shozoku="東西線"}; 
{kanji="南砂町"; kana="みなみすなまち"; romaji="minami-sunamachi"; shozoku="東西線"}; 
{kanji="東陽町"; kana="とうようちょう"; romaji="touyoucho"; shozoku="東西線"}; 
{kanji="木場"; kana="きば"; romaji="kiba"; shozoku="東西線"}; 
{kanji="門前仲町"; kana="もんぜんなかちょう"; romaji="monzen-nakacho"; shozoku="東西線"}; 
{kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="東西線"}; 
{kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="東西線"}; 
{kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="東西線"}; 
{kanji="竹橋"; kana="たけばし"; romaji="takebashi"; shozoku="東西線"}; 
{kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="東西線"}; 
{kanji="飯田橋"; kana="いいだばし"; romaji="iidabashi"; shozoku="東西線"}; 
{kanji="神楽坂"; kana="かぐらざか"; romaji="kagurazaka"; shozoku="東西線"}; 
{kanji="早稲田"; kana="わせだ"; romaji="waseda"; shozoku="東西線"}; 
{kanji="高田馬場"; kana="たかだのばば"; romaji="takadanobaba"; shozoku="東西線"}; 
{kanji="落合"; kana="おちあい"; romaji="ochiai"; shozoku="東西線"}; 
{kanji="中野"; kana="なかの"; romaji="nakano"; shozoku="東西線"}; 
{romaji="shinkiba"; kana="しんきば"; kanji="新木場"; shozoku="有楽町線"}; 
{romaji="tatsumi"; kana="たつみ"; kanji="辰巳"; shozoku="有楽町線"}; 
{romaji="toyosu"; kana="とよす"; kanji="豊洲"; shozoku="有楽町線"}; 
{romaji="tsukishima"; kana="つきしま"; kanji="月島"; shozoku="有楽町線"}; 
{romaji="shintomityou"; kana="しんとみちょう"; kanji="新富町"; shozoku="有楽町線"}; 
{romaji="ginzaittyoume"; kana="ぎんざいっちょうめ"; kanji="銀座一丁目"; shozoku="有楽町線"}; 
{romaji="yuurakutyou"; kana="ゆうらくちょう"; kanji="有楽町"; shozoku="有楽町線"}; 
{romaji="sakuradamon"; kana="さくらだもん"; kanji="桜田門"; shozoku="有楽町線"}; 
{romaji="nagatacho"; kana="ながたちょう"; kanji="永田町"; shozoku="有楽町線"}; 
{romaji="koujimachi"; kana="こうじまち"; kanji="麹町"; shozoku="有楽町線"}; 
{romaji="ichigaya"; kana="いちがや"; kanji="市ヶ谷"; shozoku="有楽町線"}; 
{romaji="iidabashi"; kana="いいだばし"; kanji="飯田橋"; shozoku="有楽町線"}; 
{kanji="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; shozoku="有楽町線"}; 
{kanji="護国寺"; kana="ごこくじ"; romaji="gokokuji"; shozoku="有楽町線"}; 
{kanji="東池袋"; kana="ひがしいけぶくろ"; romaji="higasiikebukuro"; shozoku="有楽町線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"}; 
{kanji="千川"; kana="せんかわ"; romaji="senkawa"; shozoku="有楽町線"}; 
{kanji="小竹向原"; kana="こたけむかいはら"; romaji="kotakemukaihara"; shozoku="有楽町線"}; 
{kanji="氷川台"; kana="ひかわだい"; romaji="hikawadai"; shozoku="有楽町線"}; 
{kanji="平和台"; kana="へいわだい"; romaji="heiwadai"; shozoku="有楽町線"}; 
{kanji="営団赤塚"; kana="えいだんあかつか"; romaji="eidanakakuka"; shozoku="有楽町線"}; 
{kanji="営団成増"; kana="えいだんなります"; romaji="eidannarimasu"; shozoku="有楽町線"}; 
{kanji="和光市"; kana="わこうし"; romaji="wakousi"; shozoku="有楽町線"}; 
] 
let global_ekikan_list = [ 
{kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
{kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="明治神宮前"; shuten="表参道"; keiyu="千代田線"; kyori=0.9; jikan=2}; 
{kiten="表参道"; shuten="乃木坂"; keiyu="千代田線"; kyori=1.4; jikan=3}; 
{kiten="乃木坂"; shuten="赤坂"; keiyu="千代田線"; kyori=1.1; jikan=2}; 
{kiten="赤坂"; shuten="国会議事堂前"; keiyu="千代田線"; kyori=0.8; jikan=1}; 
{kiten="国会議事堂前"; shuten="霞ヶ関"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="霞ヶ関"; shuten="日比谷"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="日比谷"; shuten="二重橋前"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="二重橋前"; shuten="大手町"; keiyu="千代田線"; kyori=0.7; jikan=1}; 
{kiten="大手町"; shuten="新御茶ノ水"; keiyu="千代田線"; kyori=1.3; jikan=2}; 
{kiten="新御茶ノ水"; shuten="湯島"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="湯島"; shuten="根津"; keiyu="千代田線"; kyori=1.2; jikan=2}; 
{kiten="根津"; shuten="千駄木"; keiyu="千代田線"; kyori=1.0; jikan=2}; 
{kiten="千駄木"; shuten="西日暮里"; keiyu="千代田線"; kyori=0.9; jikan=1}; 
{kiten="西日暮里"; shuten="町屋"; keiyu="千代田線"; kyori=1.7; jikan=2}; 
{kiten="町屋"; shuten="北千住"; keiyu="千代田線"; kyori=2.6; jikan=3}; 
{kiten="北千住"; shuten="綾瀬"; keiyu="千代田線"; kyori=2.5; jikan=3}; 
{kiten="綾瀬"; shuten="北綾瀬"; keiyu="千代田線"; kyori=2.1; jikan=4}; 
{kiten="浅草"; shuten="田原町"; keiyu="銀座線"; kyori=0.8; jikan=2}; 
{kiten="田原町"; shuten="稲荷町"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="稲荷町"; shuten="上野"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="上野"; shuten="上野広小路"; keiyu="銀座線"; kyori=0.5; jikan=2}; 
{kiten="上野広小路"; shuten="末広町"; keiyu="銀座線"; kyori=0.6; jikan=1}; 
{kiten="末広町"; shuten="神田"; keiyu="銀座線"; kyori=1.1; jikan=2}; 
{kiten="神田"; shuten="三越前"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="三越前"; shuten="日本橋"; keiyu="銀座線"; kyori=0.6; jikan=2}; 
{kiten="日本橋"; shuten="京橋"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="京橋"; shuten="銀座"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="銀座"; shuten="新橋"; keiyu="銀座線"; kyori=0.9; jikan=2}; 
{kiten="新橋"; shuten="虎ノ門"; keiyu="銀座線"; kyori=0.8; jikan=2}; 
{kiten="虎ノ門"; shuten="溜池山王"; keiyu="銀座線"; kyori=0.6; jikan=2}; 
{kiten="溜池山王"; shuten="赤坂見附"; keiyu="銀座線"; kyori=0.9; jikan=2}; 
{kiten="赤坂見附"; shuten="青山一丁目"; keiyu="銀座線"; kyori=1.3; jikan=2}; 
{kiten="青山一丁目"; shuten="外苑前"; keiyu="銀座線"; kyori=0.7; jikan=2}; 
{kiten="外苑前"; shuten="表参道"; keiyu="銀座線"; kyori=0.7; jikan=1}; 
{kiten="表参道"; shuten="渋谷"; keiyu="銀座線"; kyori=1.3; jikan=1}; 
{kiten="渋谷"; shuten="表参道"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="表参道"; shuten="青山一丁目"; keiyu="半蔵門線"; kyori=1.4; jikan=2}; 
{kiten="青山一丁目"; shuten="永田町"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="永田町"; shuten="半蔵門"; keiyu="半蔵門線"; kyori=1.0; jikan=2}; 
{kiten="半蔵門"; shuten="九段下"; keiyu="半蔵門線"; kyori=1.6; jikan=2}; 
{kiten="九段下"; shuten="神保町"; keiyu="半蔵門線"; kyori=0.4; jikan=1}; 
{kiten="神保町"; shuten="大手町"; keiyu="半蔵門線"; kyori=1.7; jikan=3}; 
{kiten="大手町"; shuten="三越前"; keiyu="半蔵門線"; kyori=0.7; jikan=1}; 
{kiten="三越前"; shuten="水天宮前"; keiyu="半蔵門線"; kyori=1.3; jikan=2}; 
{kiten="水天宮前"; shuten="清澄白河"; keiyu="半蔵門線"; kyori=1.7; jikan=3}; 
{kiten="清澄白河"; shuten="住吉"; keiyu="半蔵門線"; kyori=1.9; jikan=3}; 
{kiten="住吉"; shuten="錦糸町"; keiyu="半蔵門線"; kyori=1.; jikan=2}; 
{kiten="錦糸町"; shuten="押上"; keiyu="半蔵門線"; kyori=1.4; jikan=2}; 
{kiten="中目黒"; shuten="恵比寿"; keiyu="日比谷線"; kyori=1.; jikan=2}; 
{kiten="恵比寿"; shuten="広尾"; keiyu="日比谷線"; kyori=1.5; jikan=3}; 
{kiten="広尾"; shuten="六本木"; keiyu="日比谷線"; kyori=1.7; jikan=3}; 
{kiten="六本木"; shuten="神谷町"; keiyu="日比谷線"; kyori=1.5; jikan=3}; 
{kiten="神谷町"; shuten="霞ヶ関"; keiyu="日比谷線"; kyori=1.3; jikan=2}; 
{kiten="霞ヶ関"; shuten="日比谷"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="日比谷"; shuten="銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1}; 
{kiten="銀座"; shuten="東銀座"; keiyu="日比谷線"; kyori=0.4; jikan=1}; 
{kiten="東銀座"; shuten="築地"; keiyu="日比谷線"; kyori=0.6; jikan=2}; 
{kiten="築地"; shuten="八丁堀"; keiyu="日比谷線"; kyori=1.; jikan=2}; 
{kiten="八丁堀"; shuten="茅場町"; keiyu="日比谷線"; kyori=0.5; jikan=1}; 
{kiten="茅場町"; shuten="人形町"; keiyu="日比谷線"; kyori=0.9; jikan=2}; 
{kiten="人形町"; shuten="小伝馬町"; keiyu="日比谷線"; kyori=0.6; jikan=1}; 
{kiten="小伝馬町"; shuten="秋葉原"; keiyu="日比谷線"; kyori=0.9; jikan=2}; 
{kiten="秋葉原"; shuten="仲御徒町"; keiyu="日比谷線"; kyori=1.; jikan=1}; 
{kiten="仲御徒町"; shuten="上野"; keiyu="日比谷線"; kyori=0.5; jikan=1}; 
{kiten="上野"; shuten="入谷"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="入谷"; shuten="三ノ輪"; keiyu="日比谷線"; kyori=1.2; jikan=2}; 
{kiten="三ノ輪"; shuten="南千住"; keiyu="日比谷線"; kyori=0.8; jikan=2}; 
{kiten="南千住"; shuten="北千住"; keiyu="日比谷線"; kyori=1.8; jikan=3}; 
{kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3}; 
{kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}; 
{kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}; 
{kiten="後楽園"; shuten="本郷三丁目"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="本郷三丁目"; shuten="御茶ノ水"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="御茶ノ水"; shuten="淡路町"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="淡路町"; shuten="大手町"; keiyu="丸ノ内線"; kyori=0.9; jikan=2}; 
{kiten="大手町"; shuten="東京"; keiyu="丸ノ内線"; kyori=0.6; jikan=1}; 
{kiten="東京"; shuten="銀座"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="銀座"; shuten="霞ヶ関"; keiyu="丸ノ内線"; kyori=1.0; jikan=2}; 
{kiten="霞ヶ関"; shuten="国会議事堂前"; keiyu="丸ノ内線"; kyori=0.7; jikan=1}; 
{kiten="国会議事堂前"; shuten="赤坂見附"; keiyu="丸ノ内線"; kyori=0.9; jikan=2}; 
{kiten="赤坂見附"; shuten="四ツ谷"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="四ツ谷"; shuten="四谷三丁目"; keiyu="丸ノ内線"; kyori=1.0; jikan=2}; 
{kiten="四谷三丁目"; shuten="新宿御苑前"; keiyu="丸ノ内線"; kyori=0.9; jikan=1}; 
{kiten="新宿御苑前"; shuten="新宿三丁目"; keiyu="丸ノ内線"; kyori=0.7; jikan=1}; 
{kiten="新宿三丁目"; shuten="新宿"; keiyu="丸ノ内線"; kyori=0.3; jikan=1}; 
{kiten="新宿"; shuten="西新宿"; keiyu="丸ノ内線"; kyori=0.8; jikan=1}; 
{kiten="西新宿"; shuten="中野坂上"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="中野坂上"; shuten="新中野"; keiyu="丸ノ内線"; kyori=1.1; jikan=2}; 
{kiten="新中野"; shuten="東高円寺"; keiyu="丸ノ内線"; kyori=1.0; jikan=1}; 
{kiten="東高円寺"; shuten="新高円寺"; keiyu="丸ノ内線"; kyori=0.9; jikan=1}; 
{kiten="新高円寺"; shuten="南阿佐ヶ谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}; 
{kiten="南阿佐ヶ谷"; shuten="荻窪"; keiyu="丸ノ内線"; kyori=1.5; jikan=2}; 
{kiten="中野坂上"; shuten="中野新橋"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="中野新橋"; shuten="中野富士見町"; keiyu="丸ノ内線"; kyori=0.6; jikan=1}; 
{kiten="中野富士見町"; shuten="方南町"; keiyu="丸ノ内線"; kyori=1.3; jikan=2}; 
{kiten="市ヶ谷"; shuten="四ツ谷"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="四ツ谷"; shuten="永田町"; keiyu="南北線"; kyori=1.3; jikan=3}; 
{kiten="永田町"; shuten="溜池山王"; keiyu="南北線"; kyori=0.9; jikan=1}; 
{kiten="溜池山王"; shuten="六本木一丁目"; keiyu="南北線"; kyori=0.9; jikan=2}; 
{kiten="六本木一丁目"; shuten="麻布十番"; keiyu="南北線"; kyori=1.2; jikan=2}; 
{kiten="麻布十番"; shuten="白金高輪"; keiyu="南北線"; kyori=1.3; jikan=2}; 
{kiten="白金高輪"; shuten="白金台"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="白金台"; shuten="目黒"; keiyu="南北線"; kyori=1.3; jikan=2}; 
{kiten="市ヶ谷"; shuten="飯田橋"; keiyu="南北線"; kyori=1.1 ; jikan=2}; 
{kiten="飯田橋"; shuten="後楽園"; keiyu="南北線"; kyori=1.4 ; jikan=2}; 
{kiten="後楽園"; shuten="東大前"; keiyu="南北線"; kyori=1.3 ; jikan=3}; 
{kiten="東大前"; shuten="本駒込"; keiyu="南北線"; kyori=0.9 ; jikan=2}; 
{kiten="本駒込"; shuten="駒込"; keiyu="南北線"; kyori=1.4; jikan=2}; 
{kiten="駒込"; shuten="西ヶ原"; keiyu="南北線"; kyori=1.4; jikan=2}; 
{kiten="西ヶ原"; shuten="王子"; keiyu="南北線"; kyori=1.0; jikan=2}; 
{kiten="王子"; shuten="王子神谷"; keiyu="南北線"; kyori=1.2; jikan=2}; 
{kiten="王子神谷"; shuten="志茂"; keiyu="南北線"; kyori=1.6; jikan=3}; 
{kiten="志茂"; shuten="赤羽岩淵"; keiyu="南北線"; kyori=1.1; jikan=2}; 
{kiten="西船橋" ; shuten="原木中山"; keiyu="東西線"; kyori=1.9; jikan=3}; 
{kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1 ; jikan=2}; 
{kiten="妙典"; shuten="行徳"; keiyu="東西線"; kyori=1.3 ; jikan=2}; 
{kiten="行徳"; shuten="南行徳"; keiyu="東西線"; kyori=1.5 ; jikan=2}; 
{kiten="南行徳"; shuten="浦安" ; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="浦安" ; shuten="葛西"; keiyu="東西線"; kyori=1.9 ; jikan=2}; 
{kiten="葛西"; shuten="西葛西"; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="西葛西"; shuten="南砂町"; keiyu="東西線"; kyori=2.7 ; jikan=2}; 
{kiten="南砂町"; shuten="東陽町"; keiyu="東西線"; kyori=1.2 ; jikan=2}; 
{kiten="東陽町"; shuten="木場" ; keiyu="東西線"; kyori=0.9 ; jikan=1}; 
{kiten="木場"; shuten="門前仲町"; keiyu="東西線"; kyori=1.1 ; jikan=1}; 
{kiten="門前仲町"; shuten="茅場町"; keiyu="東西線"; kyori=1.8 ; jikan=2}; 
{kiten="茅場町"; shuten="日本橋"; keiyu="東西線"; kyori=0.5 ; jikan=1}; 
{kiten="日本橋"; shuten="大手町"; keiyu="東西線"; kyori=0.8 ; jikan=1}; 
{kiten="大手町"; shuten="竹橋"; keiyu="東西線"; kyori=1.0; jikan=2}; 
{kiten="竹橋"; shuten="九段下"; keiyu="東西線"; kyori=1.0; jikan=1}; 
{kiten="九段下"; shuten="飯田橋"; keiyu="東西線"; kyori=0.7; jikan=1}; 
{kiten="飯田橋"; shuten="神楽坂"; keiyu="東西線"; kyori=1.2; jikan=2}; 
{kiten="神楽坂"; shuten="早稲田"; keiyu="東西線"; kyori=1.2; jikan=2}; 
{kiten="早稲田"; shuten="高田馬場"; keiyu="東西線"; kyori=1.7; jikan=3}; 
{kiten="高田馬場"; shuten="落合"; keiyu="東西線"; kyori=1.9; jikan=3}; 
{kiten="落合"; shuten="中野"; keiyu="東西線"; kyori=2.0; jikan=3}; 
{kiten="新木場"; shuten="辰巳"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="辰巳"; shuten="豊洲"; keiyu="有楽町線"; kyori=1.7; jikan=2}; 
{kiten="豊洲"; shuten="月島"; keiyu="有楽町線"; kyori=1.4; jikan=2}; 
{kiten="月島"; shuten="新富町"; keiyu="有楽町線"; kyori=1.3; jikan=2}; 
{kiten="新富町"; shuten="銀座一丁目"; keiyu="有楽町線"; kyori=0.7; jikan=1}; 
{kiten="銀座一丁目"; shuten="有楽町"; keiyu="有楽町線"; kyori=0.5; jikan=1}; 
{kiten="有楽町"; shuten="桜田門"; keiyu="有楽町線"; kyori=1.0; jikan=1}; 
{kiten="桜田門"; shuten="永田町"; keiyu="有楽町線"; kyori=0.9; jikan=2}; 
{kiten="永田町"; shuten="麹町"; keiyu="有楽町線"; kyori=0.9; jikan=1}; 
{kiten="麹町"; shuten="市ヶ谷"; keiyu="有楽町線"; kyori=0.9; jikan=1}; 
{kiten="市ヶ谷"; shuten="飯田橋"; keiyu="有楽町線"; kyori=1.1; jikan=2}; 
{kiten="飯田橋"; shuten="江戸川橋"; keiyu="有楽町線"; kyori=1.6; jikan=3}; 
{kiten="江戸川橋"; shuten="護国寺"; keiyu="有楽町線"; kyori=1.3; jikan=2}; 
{kiten="護国寺"; shuten="東池袋"; keiyu="有楽町線"; kyori=1.1; jikan=2}; 
{kiten="東池袋"; shuten="池袋"; keiyu="有楽町線"; kyori=2.0; jikan=2}; 
{kiten="池袋"; shuten="要町"; keiyu="有楽町線"; kyori=1.2; jikan=2}; 
{kiten="要町"; shuten="千川"; keiyu="有楽町線"; kyori=1.0; jikan=1}; 
{kiten="千川"; shuten="小竹向原"; keiyu="有楽町線"; kyori=1.0; jikan=2}; 
{kiten="小竹向原"; shuten="氷川台"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="氷川台"; shuten="平和台"; keiyu="有楽町線"; kyori=1.4; jikan=2}; 
{kiten="平和台"; shuten="営団赤塚"; keiyu="有楽町線"; kyori=1.8; jikan=2}; 
{kiten="営団赤塚"; shuten="営団成増"; keiyu="有楽町線"; kyori=1.5; jikan=2}; 
{kiten="営団成増"; shuten="和光市"; keiyu="有楽町線"; kyori=2.1; jikan=3}; 
] 
(* 10-10 ローマ字の駅名と駅名リストを受け取ったら駅の漢字表記の文字列を返す *)
(* romaji_to_kanji string ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with
[] -> ""
| first :: rest -> if romaji = first.romaji then first.kanji
                                            else romaji_to_kanji romaji rest ;; 

(* let test1 = romaji_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原"
let test2 = romaji_to_kanji "yoyogiuehar" global_ekimei_list = ""  *)
(* 10-11 *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
(* 漢字の駅名を二つ、ekikanのリストを受け取り、隣接する二駅の距離を返す *)
(* もし隣接していない、存在していない駅が入った場合、infinityを返す *)
let rec get_ekikan_kyori x y z = match z with
[] -> infinity
| first :: rest -> if (first.kiten = x && first.shuten = y ) || (first.kiten = y && first.shuten = x) then first.kyori
                                                                                                      else get_ekikan_kyori x y rest;;
(* let test = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let test = get_ekikan_kyori "茗荷谷" "大塚" global_ekikan_list = infinity *)

(*ローマ字の駅名を受け取ったら、その間の距離を調べ、直接繋がっている場合は文字列返す  *)
(* kyori_wo_hyoji ; x :string -> y : string -> z:string *)
 let kyori_wo_hyoji x y = 
  let eki1 = romaji_to_kanji x global_ekimei_list in 
    let eki2 = romaji_to_kanji y global_ekimei_list in
      if eki1 = "" then x ^ "という駅は存在しません"
                    else if eki2 = "" then y ^ "という駅は存在しません"
      else let kyori = get_ekikan_kyori eki1 eki2 global_ekikan_list in
        if kyori = infinity then eki1 ^ "駅と" ^ eki2 ^ "駅は繋がっていません"
                            else eki1 ^ "駅から" ^ eki2 ^ "駅までは" ^ (string_of_float kyori) ^ "kmです" ;;

(* let test = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです"
let test = kyori_wo_hyoji "myogadani" "ikebukuro" = "茗荷谷駅と池袋駅は繋がっていません"
let test = kyori_wo_hyoji "myogadan" "shinotsuka" = "myogadanという駅は存在しません" *)


(* 12-2 *)
(* make_eki_list ekimei_t list -> eki_t list *)
(* let rec make_eki_list l = match l with
[] -> []
| first :: rest -> {namae = first.kanji; saitan_kyori = infinity; temae_list = []} :: make_eki_list rest;; *)

let make_eki_list l = List.map (fun s -> {namae = s.kanji; saitan_kyori = infinity; temae_list = []}) l;;
 (* let global_ekimei_short_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]
let test1 = make_eki_list global_ekimei_short_list = [{namae = "代々木上原"; saitan_kyori = infinity; temae_list = []};
{namae = "代々木公園"; saitan_kyori = infinity; temae_list = []}
]  *)

(* 12-3 *)
(* shokika eki_t list -> eki_t list *)
let shokika l eki_s = List.map(fun s -> if eki_s = s.namae then{namae = s.namae; saitan_kyori = 0.0; temae_list = [s.namae]} else s) l;;

(* 14-12 *)
(* make_initial_eki_list eki_list eki -> eki_list *)
 let make_initial_eki_list l eki_s = 
  List.map (fun s -> if eki_s = s.namae then{namae = s.namae; saitan_kyori = 0.0; temae_list = [s.namae]} else s)
  (List.map (fun s -> {namae = s.kanji; saitan_kyori = infinity; temae_list = []}) l);; 

(* let shokika l = match l with
[] -> []
| first :: rest -> {namae = first.namae; saitan_kyori = 0.0; temae_list = [first.namae]} :: rest;;
  {namae = eki.namae; saitan_kyori = 0.0; temae_list = [eki.namae]};;
let eki_yyg = {namae = "代々木上原"; saitan_kyori = infinity; temae_list = []} ;; *)
(* let test1 = shokika eki_yyg = {namae = "代々木上原"; saitan_kyori = 0.0; temae_list = ["代々木上原"]};; *)

(* 12-4 *)
(* seiretsu ekimei_t list-> ekimei_t list *)
 let rec insert lst s = match lst with
[] -> s :: []
| first :: rest -> if first.kana < s.kana then first :: (insert rest s)
                   else if first.kana = s.kana then insert rest s   
                                        else s :: lst ;;
let rec seiretsu l = match l with
[] -> []
| first :: rest -> insert (seiretsu rest) first ;;

(* let global_ekimei_short_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

let test = seiretsu global_ekimei_short_list = [
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
] *)

(* 13-6 *)
(* koushin1 eki_t -> eki_t*)

let eki_yyg = {namae = "代々木上原"; saitan_kyori = infinity; temae_list = []} ;;



let a = {namae = "a"; saitan_kyori = 1.0; temae_list = []} ;;
let b = {namae = "b"; saitan_kyori = 2.0; temae_list = []} ;;
let c = {namae = "c"; saitan_kyori = 3.0; temae_list = []} ;;
let d = {namae = "d"; saitan_kyori = 4.0; temae_list = []} ;;
let e = {namae = "e"; saitan_kyori = 5.0; temae_list = []} ;;

(*  *)

(* 13-7 *)
(* 14 - 7 *)
(* 14-13 *)
(* 16-3 *)
(* koushin defined_eki ekikan_t -> eki list -> eki list *)
let koushin p v l=
  let koushin1 p q = 
  let length = get_ekikan_kyori q.namae p.namae l in
   if length = infinity
  then q
  else let new_length = p.saitan_kyori +. length in
    if new_length > q.saitan_kyori 
      then q
      else {namae = q.namae; saitan_kyori = new_length ; temae_list = q.namae ::  p.temae_list} in
   List.map (koushin1 p) v ;;

let test = koushin a [b; c; d; e] global_ekikan_list ;;
(* 14.14 *)
(* let koushin p v =
  let koushin1 q = 
  let length = get_ekikan_kyori q.namae p.namae global_ekikan_list in
   if length = infinity
  then q
  else let new_length = p.saitan_kyori +. length in
    if new_length > q.saitan_kyori 
      then q
      else {namae = q.namae; saitan_kyori = new_length ; temae_list = q.namae ::  p.temae_list} in
   List.map (fun koushin1 q = 
  let length = get_ekikan_kyori q.namae p.namae global_ekikan_list in
   if length = infinity
  then q
  else let new_length = p.saitan_kyori +. length in
    if new_length > q.saitan_kyori 
      then q
      else {namae = q.namae; saitan_kyori = new_length ; temae_list = q.namae ::  p.temae_list} in) v ;; *)

(* 15.4 *)
(* saitan_wo_bunri eki_t lst *)
(* let rec minimum_eki l = match l with
[] -> max_float
| first :: rest -> if first.saitan_kyori < minimum_eki rest 
then first .saitan_kyori
else minimum_eki rest;;
let rec shukei_eki eki l = match l with
[] -> []
| first :: rest -> if first.saitan_kyori = eki.saitan_kyori 
then shukei_eki eki rest 
else first :: shukei_eki eki rest;;
let saitan_wo_bunri l = 
  let minimum = minimum_eki l in
  (minimum, shukei_eki minimum l);; *)

(* let saitan_bunri a kumi = match kumi with
  (b, c) -> if a.saitan_kyori < b.saitan_kyori then (a, b :: c) else (b, a :: c);; *)
let saitan_wo_bunri l = match l with
[] -> ({namae = ""; saitan_kyori = max_float; temae_list = []}, [])
| first :: rest -> List.fold_right (fun a kumi -> match kumi with
  (b, c) -> if a.saitan_kyori < b.saitan_kyori then (a, b :: c) else (b, a :: c)) rest (first, []);;

let test = saitan_wo_bunri [a; b] = (a , [b]);;

let yueki = {
  namae = "代々木上原"; saitan_kyori = 0.; temae_list = ["代々木上原"]
}

let ykeki = {
  namae = "代々木公園"; saitan_kyori = max_float; temae_list = []
}

let mjeki = {
  namae = "明治神宮前"; saitan_kyori = max_float; temae_list = []
}

(* 16-4 *)
(* dijkstra_main eki_t list ekikan_t list -> eki_t list *)
let s elst eklst = 
  let rec hojo rlst dlst = match saitan_wo_bunri rlst with
  (a, b) -> match b with
  [] -> a :: dlst
  | first :: rest -> hojo (koushin a b eklst) (a :: dlst)
  in hojo elst [];;

  let test = dijkstra_main [yueki ; ykeki; mjeki] global_ekikan_list;;

  (* 16-5 *)
  (* dijkstra string string *)
  (* seiretsu *)
let dijkstra start last =
    let s_global_ekimei_list = seiretsu global_ekimei_list in
    let start_kanji = romaji_to_kanji start s_global_ekimei_list in
    let end_kanji = romaji_to_kanji last s_global_ekimei_list in 
    let shoki_list = make_initial_eki_list s_global_ekimei_list start_kanji in
    let done_list = dijkstra_main shoki_list global_ekikan_list in
    let rec check_last lst = match lst with
    [] -> mjeki
    | first :: rest -> if first.namae = end_kanji then first
    else check_last rest in
    check_last done_list;;

let test = dijkstra "ikebukuro" "meguro";;
