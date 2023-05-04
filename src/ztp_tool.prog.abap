*&---------------------------------------------------------------------*
*& Report  ZTP_TOOL                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  ztp_tool line-size 170.                               .

types: ty_file_name(255) type c.
* Selection screen *
parameters p_trkorr like e070-trkorr  obligatory.
parameters p_lpath  type ty_file_name obligatory default 'c:\temp\'.

selection-screen skip.

parameters p_down radiobutton group func default 'X'.
parameters p_up   radiobutton group func.

* Global data *
type-pools sabc.

data pos  type i.
data temp type c.

data path        type ty_file_name.
data path_cofile type ty_file_name.
data path_data   type ty_file_name.
data path_bin    type ty_file_name.

data fk type ty_file_name.
data fr type ty_file_name.
data fd type ty_file_name.

data separator type c.

*--*
initialization.

  concatenate sy-sysid 'K' into p_trkorr.
  condense p_trkorr no-gaps.

* Complete local path *
at selection-screen.
  pos  = strlen( p_lpath ) - 1.
  temp =  p_lpath+pos(1).
  check not temp = '\'.
  concatenate p_lpath '\' into p_lpath.

start-of-selection.

* Determine transport base directory *
  call 'C_SAPGPARAM' id 'NAME'  field 'DIR_TRANS'
                     id 'VALUE' field  path.

* Determine file/path separator *
  if sy-opsys = 'Windows NT'.
    separator = '\'.
  else.
    separator = '/'.
  endif.

* Determine transport directories *
  concatenate path separator 'cofiles' separator into path_cofile.
  concatenate path separator 'data'    separator into path_data.
  concatenate path separator 'data'    separator into path_bin.
  write  / 'Paths:'.

  write:/  'Cofile...', path_cofile.
  write:/  'Data.....', path_data.
  write:/  'Bin......', path_bin.
  skip.

* Build filenames *
  concatenate p_trkorr+3(7) p_trkorr+0(3) into fk separated by '.'.
  concatenate 'R' p_trkorr+4(6) '.' p_trkorr+0(3) into fr.
  concatenate 'D' p_trkorr+4(6) '.' p_trkorr+0(3) into fd.

* Up- or download transport request *
  if p_down = 'X'.
    perform download using path_cofile p_lpath fk.
    perform download using path_data   p_lpath fr.
*   perform download using path_data   p_lpath fd.
    perform download using path_bin    p_lpath fd.
  else.
    perform upload using p_lpath path_cofile fk.
    perform upload using p_lpath path_data   fr.
*   perform upload using p_lpath path_data   fd.
    perform upload using p_lpath path_bin    fd.
  endif.

* Download files *
form download using value(srcpath) type ty_file_name
                   value(dstpath) type ty_file_name
                   value(file)    type ty_file_name.
  data begin of buffer occurs 0.
  data   data(1024) type x.
  data end of buffer.

  data filename type rlgrap-filename.
  data reclen   type i.
  data filelen  type i.

* Read input file *
  concatenate srcpath file into filename.
  open dataset filename for input in binary mode.
  check sy-subrc = 0.
  while sy-subrc = 0.
    read dataset filename into buffer length reclen.
    filelen = filelen + reclen.
    append buffer.
  endwhile.
  close dataset filename.

* Download file *
  concatenate dstpath file into filename.
  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      bin_filesize = filelen
      filename     = filename
      filetype     = 'BIN'
    TABLES
      data_tab     = buffer
    EXCEPTIONS
      others       = 1.
  check sy-subrc = 0.
  write: / 'Download:',(150) file.
endform.                    "download

* Upload file *
form upload using value(srcpath) type ty_file_name
                 value(dstpath) type ty_file_name
                 value(file)    type ty_file_name.
  data begin of buffer occurs 0.
  data   data(1024) type x.
  data end of buffer.

  data filename type rlgrap-filename.
  data reclen   type i.
  data filelen  type i.
  data result   type c.

* Check if file exists *
  concatenate srcpath file into filename.
  CALL FUNCTION 'WS_QUERY'
    EXPORTING
      filename = filename
      query    = 'FE'
    IMPORTING
      return   = result
    EXCEPTIONS
      others   = 1.
  check sy-subrc = 0 and result = '1'.

* Upload file *
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      filename   = filename
      filetype   = 'BIN'
    IMPORTING
      filelength = filelen
    TABLES
      data_tab   = buffer
    EXCEPTIONS
      others     = 1.

* Write file to server *
  concatenate dstpath file into filename.
  open dataset filename for output in binary mode.
  check sy-subrc = 0.
  loop at buffer.
*    DESCRIBE FIELD buffer-data LENGTH reclen.   "Unicode Del
    reclen = xstrlen( buffer-data ).             "Unicode Ins
    if filelen > reclen.
      filelen = filelen - reclen.
    else.
      reclen = filelen.
    endif.
    transfer buffer to filename length reclen.
  endloop.
  close dataset filename.
  write: / 'Upload:',file(150).
endform.                    "upload
