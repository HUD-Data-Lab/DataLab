for /d %%G in (*) do (
	"c:\Program Files\7-Zip\7z.exe" a "%%G.zip" ".\%%G\*"
)