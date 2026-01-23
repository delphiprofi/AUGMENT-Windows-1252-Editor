unit eAktenHandler.Intf;

interface

Uses
  eAktenSystem.Intf
, MemHandle.Intf
, FileMetadata.Intf
;

Type
  // IXMLComparer - ENTFERNT in v1.0-simplified
  // Der XML-Vergleich erfolgt nun auf D2007-Seite synchron

  IMustAbort = interface
    ['{C4E9FB58-BFB0-441E-89BE-91599CDCDCC3}']
    function MustAbort(WaitingTimeMS: Cardinal): Boolean;
  end;

  // DLL07-interne Konstanten, Typen und Interfaces sind jetzt in eAktenInternal.Intf.pas

  // ============================================================================
  // IeAktenHandler - Haupt-Interface
  // v1.0-simplified: Kein Thread, kein Callback, kein Singleton
  // ============================================================================
  IeAktenHandler = interface
    ['{D5FA90B2-B5B3-45C2-B11B-6B1ECB64F599}']
    // --- Gemeinsame Methoden (D2007 + DLL07) - MUESSEN ZUERST KOMMEN! ---
    procedure Init(const aBasePath, aKundenNr : WideString);
    function  LastError: WideString;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Prueft ob XML-Daten fuer diese Akte existieren und laedt sie.
    ///   Verwendung: Vor dem Speichern neuer XML-Daten pruefen ob bereits
    ///   Daten existieren. Falls ja, werden die Daten und die LastGUID
    ///   zurueckgegeben fuer den XML-Vergleich (nur geaenderte Daten speichern).
    ///   Falls nein, muss SaveXML mit aParentGUID=NIL aufgerufen werden.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aData">Ausgabe: Die vorhandenen XML-Daten (fuer Vergleich)</param>
    /// <param name="LastGUID">Ausgabe: GUID der letzten Version (als ParentGUID fuer SaveXML)</param>
    /// <returns>True wenn XML existiert, False wenn Akte neu angelegt werden muss</returns>
    {$ENDREGION}
    function  ExistsXML( aNrTypEx : PNrTypEx; out aData : IMemHandle; out LastGUID : TGUID ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Speichert XML-Daten in die eAkte (neue Revision).
    ///   Verwendung: Nach ExistsXML und XML-Vergleich die geaenderten Daten speichern.
    ///   - Neue Akte: aParentGUID = NIL
    ///   - Neue Revision: aParentGUID = @LastGUID aus ExistsXML
    ///   XML-Daten werden immer als neue Revision gespeichert, nie ueberschrieben.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aData">Die zu speichernden XML-Daten</param>
    /// <param name="aParentGUID">NIL fuer neue Akte, @GUID fuer neue Revision</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  SaveXML( aNrTypEx : PNrTypEx; aData : IMemHandle; aParentGUID : PGUID ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Speichert Daten in die eAkte ohne Metadaten und ohne GUID-Rueckgabe.
    ///   Verwendung: Einfache Speicherung von Dokumenten ohne Versionierung,
    ///   z.B. Import von externen PDFs oder einmalige Speicherungen wo keine
    ///   Verknuepfung zu einem RTF-Quelldokument besteht.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aBlockType">Art der Daten (btPDF, btPKCS7, etc.)</param>
    /// <param name="aDaten">Die zu speichernden Daten als IMemHandle</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  StoreToAkte( const aNrTypEx : PNrTypEx; aBlockType : TBlockType; aDaten : IMemHandle ) : Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Speichert Daten in die eAkte mit Metadaten, GUID wird generiert.
    ///   Verwendung: Wenn eine neue GUID generiert werden soll und zurueckgegeben
    ///   wird, aber kein Parent-Bezug besteht. Kann alternativ zu StoreToAkteWithGUID
    ///   verwendet werden, wenn die GUID nicht vorher bekannt sein muss.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aBlockType">Art der Daten (btPDF, btPKCS7, etc.)</param>
    /// <param name="aDaten">Die zu speichernden Daten als IMemHandle</param>
    /// <param name="aMetadata">Optionale Metadaten (kann NIL sein)</param>
    /// <param name="GUID">Ausgabe: Die generierte GUID des gespeicherten Dokuments</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  StoreToAkte( const aNrTypEx : PNrTypEx; aBlockType : TBlockType; aDaten : IMemHandle; aPageCount : Word; const aMetadata : IFileMetadata; out GUID : TGUID ) : Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Speichert Daten in die eAkte mit einer vorgegebenen GUID (ohne Parent).
    ///   Verwendung: Erstes Speichern eines RTF als PDF. Das RTF hatte noch kein
    ///   ParentGUID (neues Dokument). Vor dem Speichern wurde eine GUID generiert
    ///   und ins RTF embedded. Das PDF muss mit derselben GUID in die eAkte,
    ///   damit spaetere Aenderungen des RTF das PDF wiederfinden koennen.
    ///   Nach dem Speichern: Die verwendete GUID bleibt im RTF als ParentGUID
    ///   fuer zukuenftige Revisionen.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aBlockType">Art der Daten (btPDF, btPKCS7, etc.)</param>
    /// <param name="aDaten">Die zu speichernden Daten als IMemHandle</param>
    /// <param name="aMetadata">Optionale Metadaten (kann NIL sein)</param>
    /// <param name="aGUID">Die vorgegebene GUID unter der das Dokument gespeichert wird</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  StoreToAkteWithGUID( const aNrTypEx : PNrTypEx; aBlockType : TBlockType; aDaten : IMemHandle; aPageCount : Word; const aMetadata : IFileMetadata; const aGUID : TGUID ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Speichert Daten als Child/Revision eines bestehenden Dokuments.
    ///   Verwendung: RTF wurde geaendert und als PDF gespeichert. Das RTF hatte
    ///   bereits ein ParentGUID (von einer frueheren Version). Das PDF wird als
    ///   Child dieser Parent-Version gespeichert.
    ///   Branching-Verhalten:
    ///   - Wird das Original erneut geaendert -> neue Revision (Rev 2, Rev 3...)
    ///   - Wird eine Revision geaendert -> Child dieser Revision (Rev 1.1, 1.2...)
    ///   Nach dem Speichern: Die zurueckgegebene GUID MUSS ins RTF als neues
    ///   ParentGUID geschrieben werden, damit die naechste Aenderung korrekt
    ///   als Child dieser Version gespeichert wird.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aBlockType">Art der Daten (btPDF, btPKCS7, etc.)</param>
    /// <param name="aDaten">Die zu speichernden Daten als IMemHandle</param>
    /// <param name="aMetadata">Optionale Metadaten (kann NIL sein)</param>
    /// <param name="aParentGUID">Die GUID der Parent-Version (aus dem RTF ausgelesen)</param>
    /// <param name="GUID">Ausgabe: Die neu generierte GUID (ins RTF zurueckschreiben!)</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  StoreToAkteAsChild( const aNrTypEx : PNrTypEx; aBlockType : TBlockType; aDaten : IMemHandle; aPageCount : Word; const aMetadata : IFileMetadata; const aParentGUID : TGUID; out GUID : TGUID ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Ermittelt den belegten Speicherplatz der eAkte.
    ///   Verwendung: Fuer Statistik-Anzeige oder Speicherplatz-Ueberwachung.
    /// </summary>
    /// <param name="aJahr">0 = alle Jahre, sonst nur das angegebene Jahr</param>
    /// <returns>Belegter Speicherplatz in Bytes</returns>
    {$ENDREGION}
    function  StorageUsed( aJahr : Integer = 0 ) : Int64;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Laedt alle Dokumente einer Akte gemaess Filter.
    ///   Verwendung: Fuer eAkte-Viewer um alle Dokumente anzuzeigen.
    ///   Kann bei grossen Akten langsam sein - fuer Lazy Loading
    ///   stattdessen ListDRNummern + ListEAkteForDR verwenden.
    /// </summary>
    /// <param name="aFilter">Filterkriterien (Jahr, DR-Nummer, Typ, etc.)</param>
    /// <param name="List">Ausgabe: Liste der gefundenen Dokumente</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  ListEAkte( aFilter : PTeAkteFilter; out List : IeAkteDataList ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Ermittelt alle verfuegbaren Jahre in der eAkte.
    ///   Verwendung: Fuer Jahres-Auswahl im eAkte-Viewer.
    /// </summary>
    /// <param name="Jahre">Ausgabe: Liste der Jahre mit Eintraegen</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  ListEAkteJahre( out Jahre : IeAkteJahrListe ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Laedt nur die DR-Nummern fuer ein Jahr (schnell, Lazy Loading).
    ///   Verwendung: Erste Stufe des Lazy Loading - nur DR-Nummern laden
    ///   fuer Baumansicht, Dokumente werden erst bei Bedarf nachgeladen.
    /// </summary>
    /// <param name="aJahr">Das Jahr fuer das DR-Nummern geladen werden sollen</param>
    /// <param name="List">Ausgabe: Liste der DR-Nummern</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  ListDRNummern( aJahr : Integer; out List : IeAkteDRNrListe ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Laedt Dokumente fuer eine spezifische DR-Nummer (on-demand).
    ///   Verwendung: Zweite Stufe des Lazy Loading - wird aufgerufen wenn
    ///   User einen DR-Knoten im Baum expandiert.
    /// </summary>
    /// <param name="aFilter">Filterkriterien (muss DR-Nummer enthalten)</param>
    /// <param name="List">Ausgabe: Liste der Dokumente fuer diese DR-Nummer</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  ListEAkteForDR( aFilter : PTeAkteFilter; out List : IeAkteDataList ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Schnelle Existenz-Pruefung ohne Daten zu laden.
    ///   Verwendung: Fuer UI-Anzeige (z.B. Icon "hat eAkte") oder schnelle
    ///   Vorab-Pruefung bevor aufwendigere Operationen gestartet werden.
    ///   Laedt keine Daten, nur Boolean-Ergebnis.
    /// </summary>
    /// <param name="aNrTypEx">Identifikation der Akte (DR-Nummer, Typ, Jahr, etc.)</param>
    /// <param name="aCheckXML">True = auch XML-Block pruefen, False = nur Dokumente</param>
    /// <returns>True wenn Eintraege existieren, False wenn Akte leer/nicht vorhanden</returns>
    {$ENDREGION}
    function  ExistsInAkte( aNrTypEx : PNrTypEx; aCheckXML : Boolean = false ) : Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Laedt Dokument und Metadaten anhand des Index aus der zuletzt geladenen Liste.
    ///   Verwendung: Nach ListEAkte den gewuenschten Eintrag per Index abrufen.
    /// </summary>
    /// <param name="aIndex">Index in der zuletzt geladenen Liste (0-basiert)</param>
    /// <param name="Data">Ausgabe: Die Dokumentdaten (dekomprimiert)</param>
    /// <param name="Metadata">Ausgabe: Metadaten des Dokuments (kann NIL sein)</param>
    /// <returns>True bei Erfolg, False bei Fehler (siehe LastError)</returns>
    {$ENDREGION}
    function  GetData( aIndex : Integer; out Data : IMemHandle; out Metadata : IFileMetadata ) : Boolean; overload;

    function  GetData( const aNrTypEx : PNrTypEx; const aGUID : TGUID; out Data : IMemHandle; out Metadata : IFileMetadata ) : Boolean; overload;

    // DLL07-interne Methoden sind jetzt in IeAktenHandlerInternal (eAktenInternal.Intf.pas)
  end;

implementation

// Record-Konstruktoren für DLL07 sind jetzt in eAktenInternal.Intf.pas

end.
